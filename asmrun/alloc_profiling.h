/***********************************************************************/
/*                                                                     */
/*                               OCaml                                 */
/*                                                                     */
/*                 Mark Shinwell, Jane Street Europe                   */
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

#include "caml/mlvalues.h"

typedef enum {
  END_OF_C_FRAMES = 0ull,
  START_OF_C_FRAMES = 0xffffffffffffffffll
} backtrace_marker;

typedef union {
  void* return_address;
  uint64_t hash;
  backtrace_marker marker;
} backtrace_entry;

extern void caml_allocation_profiling_create_backtrace_stack(
    backtrace_entry** top, backtrace_entry** bottom,
    backtrace_entry** limit);

extern void caml_allocation_profiling_initialize(void);

extern intnat caml_allocation_profiling_my_profinfo(void);
