type t =
  | AccessSpecDecl
  | BlockDecl
  | CapturedDecl
  | ClassScopeFunctionSpecializationDecl
  | EmptyDecl
  | ExportDecl
  | ExternCContextDecl
  | FileScopeAsmDecl
  | FiendDecl
  | FiendTemplateDecl
  | ImportDecl
  | LifetimeExtendedTemporaryDecl
  | LinkageSpecDecl
  | LabelDecl (* 13, named decl *)
  | NamespaceDecl
  | NamespaceAliasDecl
  | ObjCCompatibleAliasDecl
  | ObjCCategoryDecl
  | ObjCCategoryImplDecl
  | ObjCImplementationDecl
  | ObjCInterfaceDecl
  | ObjCProtocolDecl
  | ObjCMethodDecl
  | ObjCPropertyDecl
  | BuiltinTemplateDecl (* 24, template decl *)
  | ConceptDecl
  | ClassTemplateDecl
  | FunctionTemplateDecl
  | TypeAliasTemplateDecl
  | VarTemplateDecl
  | TemplateTemplateParamDecl
  | EnumDecl (* 31, type decl *)
  | RecordDecl
  | CXXRecordDecl
  | ClassTemplateSpecializationDecl
  | ClassTemplatePartialSpecializationDecl
  | TemplateTypeParamDecl
  | ObjCTypeParamDecl
  | TypeAliasDecl
  | TypedefDecl
  | UnresolvedUsingTypenameDecl
  | UsingDecl
  | UsingDirectiveDecl
  | UsingPackDecl
  | UsingShadowDecl
  | BindingDecl
  | ConstructorUsingShadowDecl
  | FieldDecl (* 47, value decl *)
  | ObjCAtDefsFieldDecl
  | ObjCIvarDecl
  | FunctionDecl
  | CXXDeductionGuideDecl
  | CXXMethodDecl
  | CXXConstructorDecl
  | CXXConversionDecl
  | CXXDestructorDecl
  | MSPropertyDecl
  | NonTypeTemplateParmDecl
  | VarDecl (* 58, var decl *)
  | DecompositionDecl
  | ImplicitParamDecl
  | OMPCapturedExprDecl
  | ParmVarDecl
  | VarTemplateSpecializationDecl
  | EnumConstantDecl
  | IndirectField
  | MSGuidDecl
  | OMPDeclareMapperDecl
  | OMPDeclareReductionDecl
  | TemplateParamObjectDecl
  | UnresolvedUsingValueDecl
  | OMPAllocateDecl
  | OMPPRequiresDecl
  | OMPThreadPrivateDecl
  | OBJCPropertyImplDecl
  | PragmaCommentDecl
  | PragmaDetectionMismatchDecl
  | RequiresExprBodyDecl
  | StaticAssertDecl
  | TranslationUnitDecl
[@@deriving show]
