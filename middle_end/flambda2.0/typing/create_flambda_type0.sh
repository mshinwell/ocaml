#!/bin/bash

set -eu -o pipefail

TEMPLATE=flambda_type0.templ.ml
DELIMITER="-- module rec binding here --"

WARNING_DELIMITER="@@@ocaml.warning"

REC_BINDINGS="\
    flambda_types.rec.ml \
    flambda_type0_core.rec.ml \
    type_erase_aliases.rec.ml \
    type_printers.rec.ml \
    derived_structures/blocks.rec.ml \
    derived_structures/closure_elements.rec.ml \
    derived_structures/closure_ids.rec.ml \
    derived_structures/closures_entry_by_closure_id.rec.ml \
    derived_structures/discriminants.rec.ml \
    derived_structures/immediates.rec.ml \
    derived_structures/types_by_closure_id.rec.ml \
    env/typing_env_extension.rec.ml \
    env/typing_env_level.rec.ml \
    env/typing_env.rec.ml \
    equality/type_equality_env.rec.ml \
    equality/type_equality.rec.ml \
    meet_and_join/api_meet_and_join.rec.ml \
    meet_and_join/lattice_ops.rec.ml \
    meet_and_join/kind_independent_meet_or_join.rec.ml \
    meet_and_join/meet_and_join_fabricated.rec.ml \
    meet_and_join/meet_and_join_naked_float.rec.ml \
    meet_and_join/meet_and_join_naked_immediate.rec.ml \
    meet_and_join/meet_and_join_naked_int32.rec.ml \
    meet_and_join/meet_and_join_naked_int64.rec.ml \
    meet_and_join/meet_and_join_naked_nativeint.rec.ml \
    meet_and_join/meet_and_join_value.rec.ml \
    meet_and_join/meet_env.rec.ml \
    meet_and_join/meet_or_join.rec.ml \
    structures/product.rec.ml \
    structures/row_like.rec.ml \
    structures/trivial_row_like.rec.ml"

OUTPUT=flambda_type0.ml

echo "(* Generated automatically -- do not edit *)" > $OUTPUT
echo >> $OUTPUT

echo "# 1 \"$TEMPLATE\"" >> $OUTPUT
temp=$(mktemp)
grep --color=never -B 1000000 -- "$DELIMITER" $TEMPLATE \
    | sed '$d' >> $temp
TEMPLATE_FIRST_PART_LENGTH=$(wc -l $temp | awk '{print $1}')
cat $temp >> $OUTPUT
rm -f $temp

FIRST=1

for ML in $REC_BINDINGS; do
    MLI=${ML}i
    MODNAME=$(echo $ML | \
      sed 's/\.ml$//' | \
      sed 's:^.*/::' | \
      awk '{ print toupper(substr($0,1,1)) tolower(substr($0,2)) }' | \
      sed 's/\.rec//')

    if [ "$FIRST" = "1" ]; then
        echo "  module rec $MODNAME : sig" >> $OUTPUT
    else
        echo "  and $MODNAME : sig" >> $OUTPUT
    fi

    LINE=$(cat $MLI | grep -B 1000000 "$WARNING_DELIMITER" | wc -l)
    echo "# $(($LINE + 1)) \"$MLI\"" >> $OUTPUT

    cat $MLI \
        | grep -A 1000000 "$WARNING_DELIMITER" \
        | tail -n +2 \
        >> $OUTPUT

    echo "  end = struct" >> $OUTPUT

    LINE=$(cat $ML | grep -B 1000000 "$WARNING_DELIMITER" | wc -l)
    echo "# $(($LINE + 1)) \"$ML\"" >> $OUTPUT

    cat $ML \
      | grep -A 1000000 "$WARNING_DELIMITER" \
      | tail -n +2 \
      >> $OUTPUT

    echo "  end" >> $OUTPUT
    echo >> $OUTPUT

    FIRST=0
done

echo "# $(($TEMPLATE_FIRST_PART_LENGTH + 2)) \"$TEMPLATE\"" >> $OUTPUT
grep --color=never -A 1000000 -- "$DELIMITER" $TEMPLATE | \
    tail -n +2 >> $OUTPUT
