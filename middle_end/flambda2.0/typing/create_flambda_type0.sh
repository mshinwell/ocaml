#!/bin/bash

set -eu -o pipefail

TEMPLATE=flambda_type0_templ.ml
DELIMITER="-- module rec binding here --"

WARNING_DELIMITER="@@@ocaml.warning"

REC_BINDINGS="\
    flambda_types.ml \
    flambda_type0_core.ml \
    type_free_names.ml \
    type_printers.ml \
    derived_structures/blocks.ml \
    derived_structures/closure_elements.ml \
    derived_structures/closure_ids.ml \
    derived_structures/closures_entry_by_closure_id.ml \
    derived_structures/discriminants.ml \
    derived_structures/immediates.ml \
    derived_structures/types_by_closure_id.ml \
    env/typing_env_extension.ml \
    env/typing_env_level.ml \
    env/typing_env.ml \
    meet_and_join/api_meet_and_join.ml \
    meet_and_join/either_meet_or_join.ml \
    meet_and_join/join_env.ml \
    meet_and_join/kind_independent_meet_or_join.ml \
    meet_and_join/meet_and_join_fabricated.ml \
    meet_and_join/meet_and_join_naked_float.ml \
    meet_and_join/meet_and_join_naked_immediate.ml \
    meet_and_join/meet_and_join_naked_int32.ml \
    meet_and_join/meet_and_join_naked_int64.ml \
    meet_and_join/meet_and_join_naked_nativeint.ml \
    meet_and_join/meet_and_join_value.ml \
    meet_and_join/meet_env.ml \
    meet_and_join/meet_or_join.ml \
    structures/product.ml \
    structures/row_like.ml \
    structures/trivial_row_like.ml"

OUTPUT=flambda_type0.ml

echo "(* Generated automatically -- do not edit *)" > $OUTPUT
echo >> $OUTPUT

grep --color=never -m 1 -B 1000000 -- "$DELIMITER" $TEMPLATE \
    | head -n -1 >> $OUTPUT

FIRST=1

for ML in $REC_BINDINGS; do
    MLI=${ML}i
    MODNAME=$(echo $ML | sed 's/\.ml$//' | sed 's:^.*/::' | sed 's/^./\U\0/')

    if [ "$FIRST" = "1" ]; then
        echo "  module rec $MODNAME : sig" >> $OUTPUT
    else
        echo "  and $MODNAME : sig" >> $OUTPUT
    fi

    LINE=$(cat $MLI | grep -B 1000000 -m 1 "$WARNING_DELIMITER" | wc -l)
    echo "#file \"$MLI\"" >> $OUTPUT
    echo "#line $(($LINE + 1))" >> $OUTPUT

    cat $MLI \
        | grep -A 1000000 -m 1 "$WARNING_DELIMITER" \
        | tail -n +2 \
        | sed 's/^/    /' \
        >> $OUTPUT

    echo "  end = struct" >> $OUTPUT

    LINE=$(cat $ML | grep -B 1000000 -m 1 "$WARNING_DELIMITER" | wc -l)
    echo "#file \"$ML\"" >> $OUTPUT
    echo "#line $(($LINE + 1))" >> $OUTPUT

    cat $ML \
      | grep -A 1000000 -m 1 "$WARNING_DELIMITER" \
      | tail -n +2 \
      | sed 's/^/    /' \
      >> $OUTPUT

    echo "  end" >> $OUTPUT
    echo >> $OUTPUT

    FIRST=0
done

grep --color=never -m 1 -A 1000000 -- "$DELIMITER" $TEMPLATE | \
    tail -n +2 >> $OUTPUT
