/***********************************************************************/
/*                                                                     */
/*                               OCaml                                 */
/*                                                                     */
/*           Mark Shinwell and Leo White, Jane Street Europe           */
/*                                                                     */
/*  Copyright 2013--2015, Jane Street Group, LLC                       */
/*                                                                     */
/*  Licensed under the Apache License, Version 2.0 (the "License");    */
/*  you may not use this file except in compliance with the License.   */
/*  You may obtain a copy of the License at                            */
/*                                                                     */
/*      http://www.apache.org/licenses/LICENSE-2.0                     */
/*                                                                     */
/*  Unless required by applicable law or agreed to in writing,         */
/*  software distributed under the License is distributed on an        */
/*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       */
/*  either express or implied.  See the License for the specific       */
/*  language governing permissions and limitations under the License.  */
/*                                                                     */
/***********************************************************************/

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>
#include <math.h>
#include <sys/resource.h>

#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/intext.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/signals.h"
#include "caml/sys.h"
#include "allocation_profiling.h"
#include "stack.h"

#include "../config/s.h"

const uintnat caml_profinfo_lowest = (uintnat) 2;

c_node_type caml_allocation_profiling_classify_c_node(c_node* node)
{
  return (node->pc & 2) ? CALL : ALLOCATION;
}

c_node* caml_allocation_profiling_c_node_of_stored_pointer(value node_stored)
{
  assert(node_stored == Val_unit || Is_c_node(node_stored));
  return (node_stored == Val_unit) ? NULL : (c_node*) Hp_val(node_stored);
}

c_node* caml_allocation_profiling_c_node_of_stored_pointer_not_null(
      value node_stored)
{
  assert(Is_c_node(node_stored));
  return (c_node*) Hp_val(node_stored);
}

value caml_allocation_profiling_stored_pointer_of_c_node(c_node* c_node)
{
  value node;
  assert(c_node != NULL);
  node = Val_hp(c_node);
  assert(Is_c_node(node));
  return node;
}

CAMLprim value caml_allocation_profiling_compare_node(
      value node1, value node2)
{
  assert(!Is_in_value_area(node1));
  assert(!Is_in_value_area(node2));

  if (node1 == node2) {
    return Val_long(0);
  }
  if (node1 < node2) {
    return Val_long(-1);
  }
  return Val_long(1);
}

CAMLprim value caml_allocation_profiling_min_override_profinfo (value v_unit)
{
  return Val_long(caml_profinfo_lowest);
}

CAMLprim value caml_allocation_profiling_max_override_profinfo (value v_unit)
{
  return Val_long(PROFINFO_MASK);
}

CAMLprim value caml_allocation_profiling_unmarshal_trie (value v_channel)
{
  return caml_input_value_to_outside_heap(v_channel);
}

CAMLprim value caml_allocation_profiling_node_num_header_words(value unit)
{
  unit = unit;
  return Val_long(Node_num_header_words);
}

CAMLprim value caml_allocation_profiling_is_ocaml_node(value node)
{
  if (!(Is_ocaml_node(node) || Is_c_node(node))) {
    printf("is_ocaml_node: (value) node = %p has the wrong tag\n",
      (void*) node);
  }
  assert(Is_ocaml_node(node) || Is_c_node(node));
  return Val_bool(Is_ocaml_node(node));
}

CAMLprim value caml_allocation_profiling_ocaml_function_identifier(value node)
{
  assert(Is_ocaml_node(node));
  return caml_copy_int64((uint64_t) Decode_node_pc(Node_pc(node)));
}

CAMLprim value caml_allocation_profiling_ocaml_tail_chain(value node)
{
  assert(Is_ocaml_node(node));
  return Tail_link(node);
}

CAMLprim value caml_allocation_profiling_ocaml_classify_field(value node,
      value offset)
{
  /* Note that [offset] should always point at an initialized call or
     allocation point, by virtue of the behaviour of the function
     [caml_allocation_profiling_ocaml_node_next], below. */

  uintnat field;

  assert(!Is_block(offset));
  field = Long_val(offset);

  assert(Is_ocaml_node(node));
  assert(field >= Node_num_header_words);
  assert(field < Wosize_val(node));

  assert(Field(node, field) != Val_unit);

  switch (Call_or_allocation_point(node, field)) {
    case CALL: {
      value callee_node;
      value second_word;
      assert(field < Wosize_val(node) - 1);
      second_word = Indirect_pc_linked_list(node, field);
      assert(second_word != Val_unit);
      if (Is_block(second_word)) {
        return Val_long(4);  /* indirect call point */
      }
      callee_node = Direct_callee_node(node, field);
      if (callee_node == Val_unit) {
        return Val_long(1);  /* direct call point to uninstrumented code */
      } else if (Is_ocaml_node(callee_node)) {
        return Val_long(2);  /* direct call point to OCaml code */
      } else {
        return Val_long(3);  /* direct call point to non-OCaml code */
      }
    }

    case ALLOCATION:
      assert(field < Wosize_val(node) - 1);
      return Val_long(0);
  }

  assert(0);
}

CAMLprim value caml_allocation_profiling_ocaml_node_skip_uninitialized
      (value node, value offset)
{
  uintnat field = Long_val(offset);

  assert(Is_ocaml_node(node));
  assert(field >= Node_num_header_words);
  assert(field < Wosize_val(node));

  for (/* nothing */; field < Wosize_val(node); field++) {
    value entry;

    entry = Field(node, field);

    if (entry == Val_unit) {
      continue;
    }

    if (entry == (value) 3) /*(Encode_tail_caller_node(node))*/ {
      /* Middle word of uninitialized direct tail call point. */
      assert (field >= Node_num_header_words + 1);
      field++; /* skip the node pointer (third word of the group) */
      continue;
    }
    return Val_long(field);
  }

  return Val_long(-1);
}

CAMLprim value caml_allocation_profiling_ocaml_node_next(value node,
      value offset)
{
  uintnat field = Long_val(offset);

  assert(Is_ocaml_node(node));
  assert(field >= Node_num_header_words);
  assert(field < Wosize_val(node));

  switch (Call_or_allocation_point(node, field)) {
    case CALL: {
      value second_word;
      assert(field < Wosize_val(node) - 1);
      second_word = Indirect_pc_linked_list(node, field);
      assert(second_word != Val_unit);
      if (Is_block(second_word)) {
        /* This is an indirect call point. */
        field += 2;
      }
      else {
        /* This is a direct call point. */
        assert(field < Wosize_val(node) - 2);
        field += 3;
      }
      break;
    }

    case ALLOCATION:
      assert(field < Wosize_val(node) - 1);
      field += 2;
      break;

    default:
      assert(0);
  }

  if (field < Wosize_val(node)) {
    return caml_allocation_profiling_ocaml_node_skip_uninitialized
        (node, Val_long(field));
  }

  return Val_long(-1);
}

CAMLprim value caml_allocation_profiling_ocaml_allocation_point_program_counter
      (value node, value offset)
{
  return caml_copy_int64((int64) Decode_alloc_point_pc(
    Alloc_point_pc(node, Long_val(offset))));
}

CAMLprim value caml_allocation_profiling_ocaml_allocation_point_annotation
      (value node, value offset)
{
  return Alloc_point_profinfo(node, Long_val(offset));
}

CAMLprim value caml_allocation_profiling_ocaml_direct_call_point_call_site
      (value node, value offset)
{
  return caml_copy_int64((int64) Decode_call_point_pc(
      Direct_pc_call_site(node, Long_val(offset))));
}

CAMLprim value caml_allocation_profiling_ocaml_direct_call_point_callee
      (value node, value offset)
{
  return caml_copy_int64((int64) Decode_call_point_pc(
      Direct_pc_callee(node, Long_val(offset))));
}

CAMLprim value caml_allocation_profiling_ocaml_direct_call_point_callee_node
      (value node, value offset)
{
  return Direct_callee_node(node, Long_val(offset));
}

CAMLprim value caml_allocation_profiling_ocaml_indirect_call_point_call_site
      (value node, value offset)
{
  return caml_copy_int64((int64) Decode_call_point_pc(
    Indirect_pc_call_site(node, Long_val(offset))));
}

CAMLprim value caml_allocation_profiling_ocaml_indirect_call_point_callees
      (value node, value offset)
{
  value callees = Indirect_pc_linked_list(node, Long_val(offset));
  assert(Is_block(callees));
  assert(Is_c_node(callees));
  return callees;
}

CAMLprim value caml_allocation_profiling_c_node_is_call(value node)
{
  c_node* c_node;
  assert(node != (value) NULL);
  assert(Is_c_node(node));
  c_node = caml_allocation_profiling_c_node_of_stored_pointer_not_null(node);
  switch (caml_allocation_profiling_classify_c_node(c_node)) {
    case CALL: return Val_true;
    case ALLOCATION: return Val_false;
  }
  assert(0);
}

CAMLprim value caml_allocation_profiling_c_node_next(value node)
{
  c_node* c_node;
  assert(node != (value) NULL);
  assert(Is_c_node(node));
  c_node = caml_allocation_profiling_c_node_of_stored_pointer_not_null(node);
  assert(c_node->next == Val_unit || Is_c_node(c_node->next));
  return c_node->next;
}

CAMLprim value caml_allocation_profiling_c_node_call_site(value node)
{
  c_node* c_node;
  assert(node != (value) NULL);
  assert(Is_c_node(node));
  c_node = caml_allocation_profiling_c_node_of_stored_pointer_not_null(node);
  return caml_copy_int64((uint64_t) Decode_c_node_pc(c_node->pc));
}

CAMLprim value caml_allocation_profiling_c_node_callee_node(value node)
{
  c_node* c_node;
  assert(node != (value) NULL);
  assert(Is_c_node(node));
  c_node = caml_allocation_profiling_c_node_of_stored_pointer_not_null(node);
  assert(caml_allocation_profiling_classify_c_node(c_node) == CALL);
  /* This might be an uninitialised tail call point: for example if an OCaml
     callee was indirectly called but the callee wasn't instrumented (e.g. a
     leaf function that doesn't allocate). */
  if (Is_tail_caller_node_encoded(c_node->data.callee_node)) {
    return Val_unit;
  }
  return c_node->data.callee_node;
}

CAMLprim value caml_allocation_profiling_c_node_profinfo(value node)
{
  c_node* c_node;
  assert(node != (value) NULL);
  assert(Is_c_node(node));
  c_node = caml_allocation_profiling_c_node_of_stored_pointer_not_null(node);
  assert(caml_allocation_profiling_classify_c_node(c_node) == ALLOCATION);
  assert(!Is_block(c_node->data.profinfo));
  return c_node->data.profinfo;
}
