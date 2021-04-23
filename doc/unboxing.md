Unboxing
========

## Overview

The unboxing algorithm in Flambda 2 aims to remove unnecessary allocations
without causing bad behaviour (e.g. pushing allocations into loops).
It currently supports:

- Unboxing along non-looping control flow, within a single function, of the
  following varieties of values:
  - Boxed `float`
  - Boxed `int32`
  - Boxed `int64`
  - Boxed `nativeint`
  - Variants
  - Closures (need examples).

- Unboxing around loops, save that the corresponding allocations will only be
  removed if:
  - the first use of the value being unboxed in the loop can make use of the
    unboxed form (for example if it is a floating-point arithmetic
    operation, as opposed to something like a non-inlined function call); and
  - this same condition also holds for the first use of the value after
    the loop, on all exit paths from the loop.  (The trick of "`+. 0.`" can
    be used here; this will not be optimised away by Flambda 2.)

The unboxing code does not handle the untagging of integers.

In the future support is expected for:

- Unboxed returns from functions (e.g. returning values of variant type
  without allocating).

- Removal of allocations from loops where there does not always exist a use
  of the corresponding unboxed value subsequent to the loop.  This
  optimisation is expected to be based on partial dead code elimination.

## Control over unboxing

Unboxing depth.

## Decision procedure

The first step is computing the unboxing decision, and then in a second step,
the decision is used to compute the extra params and args for the
continuation. This is useful, in order to enable unboxing of recursive
continuation. Indeed, in the case of a recursive continuation, we will have
the following scenario: the first step (creating the unboxing decisions) is
done before entering the handler of the continuation whose parameters we want
to unbox, and thus we have access to all the non-recursive uses of the
continuation. Then, once we have finished the downwards pass on the handler
of the continuation, and thus have finally access to all uses of the
continuation, we can compute the extra params and args for all use sites.

The creation of unboxing decisions is done using the following steps:

- Using the typing env computed from the continuation uses join, we first
create an optimistic unboxing decision, based on the shape of the type of each
of the continuation parameters.

- We then do a pass on each decision which does two things:
  - pre-compute the extra args for each of the continuation use-site
  available at that point (i.e. non-recursive uses)
  - extra_args can be either already in scope, or in some cases need to add
  an extra primitive (i.e. untag, unbox, block_load, ...)

- If it is not possible to generate such an extra arg (2), remove the
problematic decision (and associated sub-decisions).

- We then check that for each decision, at least one of the extra args
computed is "already in scope", else the unboxing is not beneficial.

- Finally, using the decisions, generate the necessary denv to simplify the
handler.

The second step is simply re-doing the pass the computes the extra args,
knowing that this time we have all the uses of the continuation, and then
extending the extra params and args of the continuation.

Another significant change from the current unboxing code is concerning
variants: whereas before this PR, fields of variants are "shared" across all
tags (i.e. there is one extra param for the field 0 of all tags, one for all
fields 1 of all tags, etc...), with this PR, we generate one extra param for
each pair (field, tag) (i.e. one extra param for the field 0 of tag 0, one
extra param for field 0 of tag 1, etc...). This required to change row_like
to consider env_extensions for each possible tag (see the comment at the
beginning of unbox_continuation_params.ml for more context about why this is
needed).

Another change required was that we need a new kind of extra arg, used for
situations where we unbox a parameter, and that at one use site, it requires
adding a primitive binding (e.g. untag, unbox number, etc...), but it happens
that the use site of the continuation is as return continuation of a function
application, in which case we need to generate an intermediate wrapper
continuation in order to name the actual arg given to the continuation (i.e.
the result of the function).

## Poison values and aliases

A _poison value_ (or _invalid constant value_) is a value that will (or
should) never actually be read during execution of the program, but has to be
provided to respect arity.

There are 3 cases where we produce invalid constants:
 - in dead branches of code (i.e. when the `prove_simple` function of an
   unboxer returns `Invalid`)
 - we generate extra parameters for each field of each potential tag when
   unboxing a variant and thus at each call site we have to provide values
   for each field; in particular for fields of a tag that is not the one at
   a call-site, we must provide poison values.
 - when we recursively try to unbox a field of a variant, for which a poison
   value was generated at call site, the unboxer also generates an invalid
   constant.

Example:
```
  if b then
    (* 1 *) apply_cont k (Block (Tag 0) (foo))
  else
    (* 2 *) apply_cont k (Block (Tag 1) (foo bar))
  where k v =
  switch (tag v) with
  | 0 -> k1 (field0 v)
  | 1 -> k2 (field0 v) (field1 v)
```
When unboxing the parameter v of k, we create the following parameters:
- is_int and constant_ctor (not used in this case)
- tag
- unboxedfield_tag0_0, unboxed_field_tag1_0 and unboxed_field_tag1_1
  for the three different fields that belong to the variant.

Thus at each call site, while the value for the unboxed field of the
corresponding tag are known and usable, there can be no correct values for
the values of unboxed fields of other tags, e.g. at call site (* 1 *), the
tag of `v` is 0, and the value computed for unboxedfield_tag0_0 is `foo`, and
we know that in the handler of k (and later) the unboxedfield_tag1_0 and
unboxed_field_tag1_1 parameters will never actually be used since they should
only be used when the tag of `v` is 1. However we must still provide value to
the apply_cont.

After unboxing, we thus get the following:
```
  if b then
    let v = Block (Tag 0) (foo) in
    (* 1 *) apply_cont k v 0 foo #poison #poison
  else
    let v = Block (Tag 1) (foo bar) in
    (* 2 *) apply_cont k v 1 #poison foo bar
  where k v unboxed_tag
        unboxedfield_tag0_0 unboxedfield_tag1_0 unboxedfield_tag1_1 =
  switch tag with
  | 0 -> k1 unboxedfield_tag0_0
  | 1 -> k2 unboxedfield_tag1_0 unboxedfield_tag1_1
```
where the handler of `k` has been simplified thanks to the equations added to
the typing env by the unboxing decisions. This is done by `denv_of_decision`,
by performing meets on the typing environment between the existing type for
parameters, and the shape corresponding to the unboxing decisions
(additionally, equations about get_tag and is_int are added to the cse
environment of the denv).

In this example, a meet will be performed between:
- v = Variant (Tag 0 -> (Known 1) (foo)
                  Tag 1 -> (Known 2) (foo bar)
  the type computed by the join on the continuation uses (before unboxing)
- Variant (Tag 0 -> (Known 1) (unboxedfield_tag0_0)
           Tag 1 -> (Known 2) (unboxedfield_tag1_0 unboxedfield_tag1_1)
  the shape create from the unboxing decision
and will result an empty env_extension, and the following updated type for the
   variant:
- v = Variant (
      Tag 0 -> (Known 1) (foo) (env_extension (foo = unboxedfield_tag0_0))
      Tag 1 -> (Known 2) (foo bar) (env_extension (foo = unboxedfield_tag1_0)
                                                  (bar = unboxedfield_tag1_1))
     )

The handler of `k` is then simplified, and after a switch on the tag of the
variant, the env_extension corresponding to the tag is added to each branch's
typing environment. In each branch, the loads to access the variant's fields
can be resolved to the unboxed parameters added, eliminating reads, and in
this case allowing to remove the parameter `v`, and thus also remove its
allocation before the apply_cont to k.

Note that simply adding the three following equations
```
      (foo = unboxedfield_tag0_0)
      (foo = unboxedfield_tag1_0)
      (bar = unboxedfield_tag1_1)
```
to the typing env in `denv_of_decision` would be incorrect. This is because
with these three equations at the same time, unboxedfield_tag0_0 and
unboxedfield_tag1_0 would become aliased, and thus the simplifier would be
allowed to substitute unboxedfield_tag0_0 to unboxedfield_tag1_0 anywhere in
the handler, which is incorrect (and will lead to segfaults !). This problem
is avoided in the current code because when performing the meet between the
param type and the unboxing shape, the env_extension returned by row_like is
empty because it is the join of the env_extensions for each tag. We then
never open together the env_extensions that are specific to each tag.

Now consider that a second pass of unboxing is performed on `k`. Before that
second unboxing decision, the code will look like this:
```
   if b then
      (* 1 *) apply_cont k 0 foo #poison #poison
   else
      (* 2 *) apply_cont k 1 #poison foo bar
   where k unboxed_tag
    unboxedfield_tag0_0
    unboxedfield_tag1_0 unboxedfield_tag1_1 =
   switch tag with
   | 0 -> k1 unboxedfield_tag0_0
   | 1 -> k2 unboxedfield_tag1_0 unboxedfield_tag1_1
```

We need here be careful about the handling of the `#poison` values with
regards to the join performed at the continuation use sites. More
specifically, while #poison may be locally considered as bottom on values
during the join, it cannot be considered as bottom when it comes to aliases,
otherwise we could end up in a situation where unboxedfield_tag0_0 and
unboxedfield_tag1_0 become aliased.

Currently, the values generated by this module are constants of the correct
kind, with no special behavior with regards to type. The joins will thus use
the actual runtime value, and avoid any problem with invalid aliases.
However, this leads to a loss of precision for the types of unboxed
parameters, and often prevent further unboxing. A dedicated poison
value/constant might enable the join to avoid this loss of precision, *but*
one should be careful that such poisons do not introduce aliases as
described above.

In these cases, some information could be recovered by attaching
env_extension to each branch of a switch on the tag of the variant. This is
not implemented yet.
