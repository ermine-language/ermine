#!/usr/bin/sh
find src -name "*.hs" -print | xargs graphmod --no-cluster -C Ermine.Syntax -C Ermine.Pretty -C Ermine.Inference -C Ermine.Builtin -C Ermine.Unification -C Ermine.Parser -C Ermine.Console -C Ermine.Binary -C Ermine.Pattern -C Ermine.Constraint | dot -Tpng > images/overview.png
