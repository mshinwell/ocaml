/***********************************************************************/
/*                                                                     */
/*                               OCaml                                 */
/*                                                                     */
/*                 Mark Shinwell, Jane Street Europe                   */
/*                                                                     */
/*  Copyright 2013--2015, Jane Street Holding                          */
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
#ifndef __APPLE__
#include <elf.h>
#endif
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>
#include <math.h>

#include "alloc.h"
#include "gc.h"
#include "major_gc.h"
#include "memory.h"
#include "minor_gc.h"
#include "mlvalues.h"
#include "signals.h"

/* Determine the byte offset of a given section in an ELF file. */
CAMLprim value
ocaml_dwarf_byte_offset_of_elf_section(value v_fd,
                                       value v_section_name)
{
#ifndef __APPLE__
  int fd;
  int bytes_read;
  char* section_name;
  Elf64_Ehdr elf_header;
  Elf64_Half section_header_index;
  off_t section_header_string_table_file_offset;
  off_t file_offset;
  Elf64_Shdr section_header;
  off_t found_section_at_offset = -1;

  assert(!Is_block(v_fd) && Int_val(v_fd) >= 0);
  assert(Is_block(v_section_name) && Tag_val(v_section_name) == String_tag);

  fd = Long_val(v_fd);
  section_name = strdup(String_val(v_section_name));
  if (section_name == NULL) {
    return Val_long(-1);
  }

  caml_enter_blocking_section();

  /* Make sure the file pointer is at the beginning, and read the ELF file
     header. */
  if (lseek(fd, 0, SEEK_SET) < 0) {
    free(section_name);
    caml_leave_blocking_section();
    return Val_long(-1);
  }
  bytes_read = read(fd, &elf_header, sizeof(elf_header));
  /* CR mshinwell: extend to support 32-bit executables */
  if (bytes_read < sizeof(elf_header)
      || elf_header.e_ident[EI_CLASS] != ELFCLASS64
      || elf_header.e_shentsize != sizeof(Elf64_Shdr)) {
    free(section_name);
    caml_leave_blocking_section();
    return Val_long(-1);
  }

  /* Read the section header for the section header string table. */
  file_offset =
    elf_header.e_shoff + sizeof(section_header)*elf_header.e_shstrndx;
  if (lseek(fd, file_offset, SEEK_SET) < 0
      || read(fd, &section_header, sizeof(section_header))
           < sizeof(section_header)) {
    free(section_name);
    caml_leave_blocking_section();
    return Val_long(-1);
  }
  section_header_string_table_file_offset = section_header.sh_offset;

  /* Iterate over each section looking for the desired section by name.  The
     name of each section is determined by reading a piece of the section
     header string table. */
  for (section_header_index = 0;
       found_section_at_offset == -1
         && section_header_index < elf_header.e_shnum;
       section_header_index++) {
    char* desired_section_name;
    int name_of_this_section_does_not_match;

    if (lseek(fd, elf_header.e_shoff
                + sizeof(section_header)*section_header_index,
              SEEK_SET) < 0
        || read(fd, &section_header,
                  sizeof(section_header)) < sizeof(section_header)
        || lseek(fd, section_header_string_table_file_offset
                   + section_header.sh_name,
                 SEEK_SET) < 0) {
      free(section_name);
      caml_leave_blocking_section();
      return Val_long(-1);
    }

    desired_section_name = section_name;
    name_of_this_section_does_not_match = 0;
    while (!name_of_this_section_does_not_match && *desired_section_name) {
      char ch;
      if (read(fd, &ch, 1) < 1) {
        free(section_name);
        caml_leave_blocking_section();
        return Val_long(-1);
      }
      if (ch != *desired_section_name++) {
        name_of_this_section_does_not_match = 1;
      }
    }

    if (!name_of_this_section_does_not_match) {
      found_section_at_offset = section_header.sh_offset;
    }
  }

  free(section_name);
  caml_leave_blocking_section();

  return Val_long(found_section_at_offset);
#else
  /* Mac OS X does not use ELF. */
  v_executable = v_executable;
  v_section_name = v_section_name;
  return Val_long(-1);
#endif
}
