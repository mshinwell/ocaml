#!/bin/sh

set -eu -o pipefail

# Needs trailing slash
install_dir=/Users/mark/dev/mshinwell-ocaml4-install/

init_script () {
  script=$1

  cat >$script <<EOF
#!/bin/sh
set -eu -o pipefail

install () {
  echo "INSTALL \$*"
}
EOF
}

process_make_output () {
  grep -v ^mkdir \
  | grep -v ^rm \
  | sed 's:/usr/bin/install.*o=[rx]* :install :' \
  | sed "s:$install_dir::" \
  | sed 's/test -f .*;/true/' \
  | sed 's:/Library.*/usr/bin/make:echo MAKE:' \
  | sed 's:ln -sf:echo LINK:' \
  | sed 's:ranlib:echo RANLIB:' \
  | sed 's:cd:echo CD:' \
  | sed 's:if true *then:if true; then:' \
  | sed 's:if true else:if false; then:' \
  | sed 's:if true fi::' \
  >> $script
}

conclude_script () {
  script=$1

  chmod +x $script
  echo "Wrote $script"
}

run_make () {
  dir=$1
  shift 2

  script=$dir/install_rules.sh

  init_script $script

  make -n -C $dir $* \
    | process_make_output
    >> $script

  conclude_script $script

  rules=$dir/dune_install_rules.sexp

  $script | egrep 'INSTALL|LINK' > $rules

  echo "Generated $rules"
}

run_make . install installopt installoptopt
run_make runtime install installopt
run_make stdlib install installopt
run_make tools install installopt
#run_make man install
run_make otherlibs/dynlink install installopt
run_make otherlibs/unix install installopt
run_make otherlibs/bigarray install installopt
run_make otherlibs/str install installopt
run_make otherlibs/raw_spacetime_lib install installopt
run_make otherlibs/systhreads install installopt
run_make ocamldoc install installopt installopt_really
#run_make debugger install -- this is only ocamldebug -> bin/ocamldebug
