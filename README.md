# Arc Macro Expander

<dl>
 <dt>arc/ssyntax.arc</dt>
 <dd>
  ssyntax from Arc<sub>3.1</sub>’s ac.scm ported to Arc.
 </dd>

 <dt>arc/macro.arc</dt>
 <dd>
  The macro expander.
 </dd>

 <dt>arc/ac-racket.arc</dt>
 <dd>
  Demo Arc to Racket cross compiler.
 </dd>

 <dt>ar.rkt</dt>
 <dd>
  Demo Racket runtime which is the target of the cross compiler.
 </dd>

 <dt>qq</dt>
 <dd>
  Quasiquotation expansion ported from clisp to Arc by fallintothis.
 </dd>

 <dt>common-tests</dt>
 <dd>
  Tests that can be run both in Arc<sub>3.1</sub> and in the subset of
  Arc needed to implement the macro expander and compiler.
 </dd>

 <dt>runtime-tests</dt>
 <dd>
  Tests that can only be run in the target runtime because they use
  features not implemented by Arc<sub>3.1</sub>.
 </dd>

 <dt>more-tests</dt>
 <dd>
  Tests for code loaded later in Arc, features not implemented by the
  subset need by the cross compiler.
 </dd>

 <dt>precompile.load-order</dt>
 <dd>
  Specifies the order in which to load the code to bootstrap the implementation of Arc.
 </dd>

 <dt>precompile.arc</dt>
 <dd>
  Run the cross compiler to generate Racket source code (serialized to
  include injected values), which implements enough of Arc so that the
  macro expander and compiler can be run in the target runtime.
 </dd>
</dl>

To run the cross compiler:

    racket -f precompile.scm

this will produce `/tmp/arc.precompiled`, as well as running tests
during cross compilation (to try to catch bugs as early as possible,
before the added complication of the running deserialized cross
compiled code in a different runtime).

And then:

    racket ar.rkt

will load `/tmp/arc.precompiled`, and run tests again.
