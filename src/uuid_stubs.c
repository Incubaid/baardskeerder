/*
 * This file is part of Baardskeerder.
 *
 * Copyright (C) 2012 Incubaid BVBA
 *
 * Baardskeerder is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Baardskeerder is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Baardskeerder.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <string.h>

#include <uuid/uuid.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>

#define UUID_T_SIZE (sizeof(uuid_t))

CAMLprim value _bs_uuid_generate(value unit) {
        CAMLparam1(unit);
        CAMLlocal1(ret);

        uuid_t uu;

        uuid_generate(uu);

        if(uuid_is_null(uu)) {
                caml_failwith("uuid_is_null");
        }

        ret = caml_alloc_string(UUID_T_SIZE);
        memcpy(String_val(ret), uu, UUID_T_SIZE);

        CAMLreturn(ret);
}

CAMLprim value _bs_uuid_unparse_upper(value uuid) {
        CAMLparam1(uuid);
        CAMLlocal1(ret);

        char buffer[64]; /* Slightly oversized */

        uuid_t uu;

        /* For safety (even though uuid_unparse_upper shouldn't modify the
         * memory of the first argument... */
        memcpy(&uu, String_val(uuid), UUID_T_SIZE);
        uuid_unparse_upper(uu, buffer);

        ret = caml_copy_string(buffer);

        CAMLreturn(ret);
}
