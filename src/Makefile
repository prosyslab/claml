CLANGLIBS=-lclangFrontend -lclangDriver -lclangSerialization -lclangParse -lclangSema -lclangAnalysis -lclangARCMigrate -lclangRewrite -lclangEdit -lclangAST -lclangLex -lclangBasic

all:
	clang-12 -I/usr/include/clang \
		`llvm-config-12 --cxxflags --ldflags --libs all` \
		clang_ocaml.cpp $(CLANGLIBS) -lstdc++ -fno-rtti

temp:
	clang-12 -I/usr/include/clang \
		`llvm-config-12 --cxxflags --ldflags --libs all` \
		temp.cpp $(CLANGLIBS) -lstdc++ -fno-rtti

