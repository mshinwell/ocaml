/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Contributed by Tracy Camp, PolyServe Inc., <campt@polyserve.com>     */
/*                                                                        */
/*   Copyright 2002 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <stdio.h>
#include <caml/mlvalues.h>
#include "unixsupport.h"

CAMLprim value unix_rename(value path1, value path2)
{
  caml_unix_check_path(path1, "rename");
  caml_unix_check_path(path2, "rename");
  if (! MoveFileEx(String_val(path1), String_val(path2),
                   MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH |
                   MOVEFILE_COPY_ALLOWED)) {
    win32_maperr(GetLastError());
    uerror("rename", path1);
  }
  return Val_unit;
}
