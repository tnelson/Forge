'(AlloyModule
  (ModuleDecl (QualName "tour" "addressBook1"))
  (SigDecl (NameList "Name" "Addr"))
  (SigDecl
   (NameList "Book")
   (Decl
    (NameList "addr")
    (Expr
     (Expr (QualName "Name"))
     (ArrowOp "->" "lone")
     (Expr (QualName "Addr")))))
  (PredDecl
   (Name "show")
   (ParaDecls (Decl (NameList "b") (Expr (QualName "Book"))))
   (Block
    (Expr
     (Expr "#" (Expr (Expr (QualName "b")) "." (Expr (QualName "addr"))))
     ">"
     (Expr (Const 1)))
    (Expr
     (Expr
      "#"
      (Expr
       (Expr (QualName "Name"))
       "."
       (Expr (Expr (QualName "b")) "." (Expr (QualName "addr")))))
     ">"
     (Expr (Const 1)))))
  (CmdDecl "run" (QualName "show") (Scope 3 (Typescope 1 (QualName "Book"))))
  (PredDecl
   (Name "add")
   (ParaDecls
    (Decl (NameList "b" "b'") (Expr (QualName "Book")))
    ","
    (DeclList
     (Decl (NameList "n") (Expr (QualName "Name")))
     ","
     (DeclList (Decl (NameList "a") (Expr (QualName "Addr"))))))
   (Block
    (Expr
     (Expr (Expr (QualName "b'")) "." (Expr (QualName "addr")))
     "="
     (Expr
      (Expr (Expr (QualName "b")) "." (Expr (QualName "addr")))
      "+"
      (Expr (Expr (QualName "n")) (ArrowOp "->") (Expr (QualName "a")))))))
  (PredDecl
   (Name "del")
   (ParaDecls
    (Decl (NameList "b" "b'") (Expr (QualName "Book")))
    ","
    (DeclList (Decl (NameList "n") (Expr (QualName "Name")))))
   (Block
    (Expr
     (Expr (Expr (QualName "b'")) "." (Expr (QualName "addr")))
     "="
     (Expr
      (Expr (Expr (QualName "b")) "." (Expr (QualName "addr")))
      "-"
      (Expr (Expr (QualName "n")) (ArrowOp "->") (Expr (QualName "Addr")))))))
  (FunDecl
   (Name "lookup")
   (ParaDecls
    (Decl (NameList "b") (Expr (QualName "Book")))
    ","
    (DeclList (Decl (NameList "n") (Expr (QualName "Name")))))
   (Expr "set" (Expr (QualName "Addr")))
   (Expr
    (Expr (QualName "n"))
    "."
    (Expr (Expr (QualName "b")) "." (Expr (QualName "addr")))))
  (PredDecl
   (Name "showAdd")
   (ParaDecls
    (Decl (NameList "b" "b'") (Expr (QualName "Book")))
    ","
    (DeclList
     (Decl (NameList "n") (Expr (QualName "Name")))
     ","
     (DeclList (Decl (NameList "a") (Expr (QualName "Addr"))))))
   (Block
    (Expr
     (Expr (QualName "add"))
     "["
     (ExprList (QualName "b") (QualName "b'") (QualName "n") (QualName "a"))
     "]")
    (Expr
     (Expr
      "#"
      (Expr
       (Expr (QualName "Name"))
       "."
       (Expr (Expr (QualName "b'")) "." (Expr (QualName "addr")))))
     ">"
     (Expr (Const 1)))))
  (CmdDecl
   "run"
   (QualName "showAdd")
   (Scope 3 (Typescope 2 (QualName "Book"))))
  (AssertDecl
   "assert"
   (Name "delUndoesAdd")
   (Block
    (Expr
     (Quant "all")
     (DeclList
      (Decl (NameList "b" "b'" "b\"") (Expr (QualName "Book")))
      ","
      (DeclList
       (Decl (NameList "n") (Expr (QualName "Name")))
       ","
       (DeclList (Decl (NameList "a") (Expr (QualName "Addr"))))))
     (BlockOrBar
      "|"
      (Expr
       (Expr
        (Expr
         "no"
         (Expr
          (Expr (QualName "n"))
          "."
          (Expr (Expr (QualName "b")) "." (Expr (QualName "addr")))))
        "and"
        (Expr
         (Expr (QualName "add"))
         "["
         (ExprList
          (QualName "b")
          (QualName "b'")
          (QualName "n")
          (QualName "a"))
         "]"))
       "and"
       (Expr
        (Expr
         (Expr (QualName "del"))
         "["
         (ExprList (QualName "b'") (QualName "b\"") (QualName "n"))
         "]")
        "implies"
        (Expr
         (Expr (Expr (QualName "b")) "." (Expr (QualName "addr")))
         "="
         (Expr (Expr (QualName "b\"")) "." (Expr (QualName "addr"))))))))))
  (AssertDecl
   "assert"
   (Name "addIdempotent")
   (Block
    (Expr
     (Quant "all")
     (DeclList
      (Decl (NameList "b" "b'" "b\"") (Expr (QualName "Book")))
      ","
      (DeclList
       (Decl (NameList "n") (Expr (QualName "Name")))
       ","
       (DeclList (Decl (NameList "a") (Expr (QualName "Addr"))))))
     (BlockOrBar
      "|"
      (Expr
       (Expr
        (Expr (QualName "add"))
        "["
        (ExprList (QualName "b") (QualName "b'") (QualName "n") (QualName "a"))
        "]")
       "and"
       (Expr
        (Expr
         (Expr (QualName "add"))
         "["
         (ExprList
          (QualName "b'")
          (QualName "b\"")
          (QualName "n")
          (QualName "a"))
         "]")
        "implies"
        (Expr
         (Expr (Expr (QualName "b'")) "." (Expr (QualName "addr")))
         "="
         (Expr (Expr (QualName "b\"")) "." (Expr (QualName "addr"))))))))))
  (AssertDecl
   "assert"
   (Name "addLocal")
   (Block
    (Expr
     (Quant "all")
     (DeclList
      (Decl (NameList "b" "b'") (Expr (QualName "Book")))
      ","
      (DeclList
       (Decl (NameList "n" "n'") (Expr (QualName "Name")))
       ","
       (DeclList (Decl (NameList "a") (Expr (QualName "Addr"))))))
     (BlockOrBar
      "|"
      (Expr
       (Expr
        (Expr (QualName "add"))
        "["
        (ExprList (QualName "b") (QualName "b'") (QualName "n") (QualName "a"))
        "]")
       "and"
       (Expr
        (Expr (Expr (QualName "n")) "!" "=" (Expr (QualName "n'")))
        "implies"
        (Expr
         (Expr
          (Expr (QualName "lookup"))
          "["
          (ExprList (QualName "b") (QualName "n'"))
          "]")
         "="
         (Expr
          (Expr (QualName "lookup"))
          "["
          (ExprList (QualName "b'") (QualName "n'"))
          "]"))))))))
  (CmdDecl
   "check"
   (QualName "delUndoesAdd")
   (Scope 10 (Typescope 3 (QualName "Book"))))
  (CmdDecl "check" (QualName "addIdempotent") (Scope 3))
  (CmdDecl
   "check"
   (QualName "addLocal")
   (Scope 3 (Typescope 2 (QualName "Book")))))


