import org.apache.bcel.generic.*;

import java.io.*;
import java.util.*;

import static org.apache.bcel.Const.ACC_PUBLIC;
import static org.apache.bcel.Const.ACC_STATIC;
import static org.apache.bcel.Const.ACC_SUPER;

class Tree {
    ArrayList<Tree> children;
    String op;
    Integer symbolId;
    Type type;

    Tree(String op) {
        children = new ArrayList<>();
        this.op = op;
    }

    void addChild(Tree child) {
        children.add(child);
    }
}

class Leaf extends Tree {
    Token token;

    Leaf(Token t) {
        super(Utils.TokentoString(t));
        token = t;
    }
}

class Utils {
    static boolean charIn(String str, char c) {
        return str.indexOf(c) >= 0;
    }

    static boolean isWhitespace(char c) {
        return charIn(" \t\n", c);
    }

    static boolean isDigit(char c) {
        return '0' <= c && c <= '9';
    }

    static boolean isDelimeter(char c) {
        return charIn(",;.", c);
    }

    static boolean isCompOrAssign(char c) {
        return charIn("=<>", c);
    }

    static boolean isParen(char c) {
        return charIn("()[]", c);
    }

    static boolean isMath(char c) {
        return charIn("*+-/%", c);
    }

    static boolean isLetter(char c) {
        return Character.isAlphabetic(c);
    }

    static String TokentoString(Token t) {
        switch (t.type) {
            case EPSILON:
                return "ε";
            default:
                return t.lexeme;
        }
    }
}

enum TokenType {
    WS, NUM, COMMA, SEMICOLON, PERIOD, LT, GT, LE, GE, NE, EQ, ASSIGN, OPAREN, CPAREN, OSPAREN, CSPAREN, PLUS, MINUS, MULT, DIVIDE, MOD, ID, TYPE_INT, TYPE_DOUBLE, ERROR, DEF, IF, FED, FI, THEN, ELSE, WHILE, DO, OD, PRINT, RETURN, OR, AND, NOT, EPSILON
}

class Token {
    TokenType type;
    String lexeme;

    Token(TokenType type) {
        this.type = type;
        this.lexeme = "";
    }

    public void addChar(char c) {
        lexeme += c;
    }

    public String toString() {
        return "<" + type + ", '" + lexeme + "'>";
    }
}

class Lex {
    private InputStream in;

    Lex() {
        //String source = "int x,i;\nx=0;i=1;\nwhile(i<10) do\n  x = x+i*i; i=i+1\nod;\nprint(x);.";
        String source = "def double f(double x, double y)\n" +
            "\tdouble a;\n" +
            "\ta=x+y*y;\n" +
            "\treturn a\n" +
            "fed;\n" +
            "double s;\ns=f(2.4,1)+f(1.3,-2.4)+f(1.0,0.1);\n" +
            "print(s).";
        //System.out.println(source);
        InputStream stringIn = new ByteArrayInputStream(source.getBytes());
        in = System.in;
    }

    private char readChar() {
        try {
            return (char) in.read();
        } catch (Exception e) {
            e.printStackTrace();
            return (char) -1;
        }
    }

    private char lookAhead() {
        try {
            in.mark(1);
            char c = (char) in.read();
            in.reset();
            return c;
        } catch (Exception e) {
            e.printStackTrace();
            return (char) -1;
        }
    }

    Token whitespace() {
        Token token = new Token(TokenType.WS);
        while (Utils.isWhitespace(lookAhead())) {
            token.addChar(readChar());
        }
        return token;
    }

    Token integer() {
        Token token = new Token(TokenType.NUM);
        if (lookAhead() == '-') {
            token.addChar(readChar());
        }
        while (Utils.isDigit(lookAhead())) {
            token.addChar(readChar());
        }
        char ll = lookAhead();
        if (ll == '.') {
            token.addChar(readChar());
            while (Utils.isDigit(lookAhead())) {
                token.addChar(readChar());
            }
        }
        if (!(Utils.isDelimeter(ll) || Utils.isWhitespace(ll) || Utils.isMath(ll) || Utils.isParen(ll))) {
            token.addChar(readChar());
            token.type = TokenType.ERROR;
        }
        return token;
    }

    Token delimeter() {
        String[] lexemes = {",", ";", "."};
        TokenType[] types = {TokenType.COMMA, TokenType.SEMICOLON, TokenType.PERIOD};

        Token token = new Token(TokenType.ERROR);
        token.addChar(readChar());
        for (int i = 0; i < lexemes.length; i++) {
            if (lexemes[i].equals(token.lexeme)) {
                token.type = types[i];
                break;
            }
        }
        return token;
    }

    Token compOrAssign() {
        Token token = new Token(TokenType.ERROR);
        char c = readChar();
        token.addChar(c);
        char ll = lookAhead();
        if ((c == '<' && Utils.charIn(">=", ll)) || (c == '>' && Utils.charIn("<=", ll))) {
            token.addChar(readChar());
        } else if (c == '=') {
            if (ll == '=') {
                token.addChar(readChar());
            }
        }

        String[] lexemes = {"<", ">", "<=", ">=", "<>", "==", "="};
        TokenType[] types = {TokenType.LT, TokenType.GT, TokenType.LE, TokenType.GE, TokenType.NE, TokenType.EQ, TokenType.ASSIGN};

        for (int i = 0; i < lexemes.length; i++) {
            if (lexemes[i].equals(token.lexeme)) {
                token.type = types[i];
                break;
            }
        }
        return token;
    }

    Token paren() {
        Token token = new Token(TokenType.ERROR);
        token.addChar(readChar());

        String[] lexemes = {"(", ")", "[", "]"};
        TokenType[] types = {TokenType.OPAREN, TokenType.CPAREN, TokenType.OSPAREN, TokenType.CSPAREN};

        for (int i = 0; i < lexemes.length; i++) {
            if (token.lexeme.equals(lexemes[i])) {
                token.type = types[i];
                break;
            }
        }
        return token;
    }

    Token math() {
        Token token = new Token(TokenType.ERROR);
        token.addChar(readChar());

        String[] lexemes = {"+", "-", "*", "/", "%"};
        TokenType[] types = {TokenType.PLUS, TokenType.MINUS, TokenType.MULT, TokenType.DIVIDE, TokenType.MOD};

        for (int i = 0; i < lexemes.length; i++) {
            if (token.lexeme.equals(lexemes[i])) {
                token.type = types[i];
                break;
            }
        }
        return token;
    }

    Token id() {
        Token token = new Token(TokenType.ID);
        token.addChar(readChar());
        while (Utils.isLetter(lookAhead()) || Utils.isDigit(lookAhead())) {
            token.addChar(readChar());
        }
        return token;
    }

    Token getNextToken() {
        char c = lookAhead();
        if (Utils.isWhitespace(c)) {
            whitespace();
            return getNextToken();
        }
        if (Utils.isDigit(c)) return integer();
        if (Utils.isDelimeter(c)) return delimeter();
        if (Utils.isCompOrAssign(c)) return compOrAssign();
        if (Utils.isParen(c)) return paren();
        if (Utils.isMath(c)) return math();


        if (Utils.isLetter(c)) {
            Token token = id();
            String[] lexemes = {"def", "if", "fed", "fi", "then", "else", "while", "do", "od", "print", "return", "or", "and", "not", "int", "double"};
            TokenType[] types = {TokenType.DEF, TokenType.IF, TokenType.FED, TokenType.FI, TokenType.THEN, TokenType.ELSE, TokenType.WHILE, TokenType.DO, TokenType.OD, TokenType.PRINT, TokenType.RETURN, TokenType.OR, TokenType.AND, TokenType.NOT, TokenType.TYPE_INT, TokenType.TYPE_DOUBLE};

            for (int i = 0; i < lexemes.length; i++) {
                if (lexemes[i].equals(token.lexeme)) {
                    token.type = types[i];
                    break;
                }
            }
            return token;
        }
        return null;
    }
}

class Parser {
    Lex lexer;
    Token buffer;
    ArrayList<FunctionSymbol> funcSymbolTable = new ArrayList<>();
    int currFunc = -1;
    HashMap<Integer, SymbolTable> symbolTables = new HashMap<>();
    Tree ast;

    Parser() {
        lexer = new Lex();
        symbolTables.put(0, new SymbolTable());
    }

    private int getFuncSymbolId(String fname) {
        for (int i = 0; i < funcSymbolTable.size(); i++) {
            FunctionSymbol fSymbol = funcSymbolTable.get(i);
            if (fSymbol.fname.equals(fname)) {
                return i + 1;
            }
        }
        throw new IllegalStateException("Function name doesn't exist");
    }

    private Token getNextToken() {
        Token result;
        if (buffer != null) {
            result = buffer;
        } else {
            result = lexer.getNextToken();
        }
        buffer = null;
        System.out.print(" " + result.lexeme);
        if (in(new TokenType[]{TokenType.SEMICOLON, TokenType.DO, TokenType.PERIOD}, result.type)) {
            System.out.print("\n// ");
        }
        return result;
    }

    private Token lookAhead() {
        if (buffer != null) {
            return buffer;
        } else {
            buffer = lexer.getNextToken();
            return buffer;
        }
    }

    private Tree epsilon() {
        Token t = new Token(TokenType.EPSILON);
        t.lexeme = "ε";
        return new Leaf(t);
    }

    private Tree fdecls(Tree parent) {
        // <fdecls> ::= <fdec>; <fdecls> | ε
        TokenType[] FDEC_FIRST = {TokenType.DEF};
        TokenType[] FOLLOW = {TokenType.TYPE_INT, TokenType.TYPE_DOUBLE, TokenType.IF, TokenType.WHILE, TokenType.PRINT, TokenType.RETURN, TokenType.ID, TokenType.SEMICOLON};

        if (parent == null) {
            parent = new Tree("fdecls");
        }

        Token ll = lookAhead();
        if (in(FDEC_FIRST, ll.type)) {
            //<fdec>; <fdecls>
            parent.addChild(fdec());
            match(TokenType.SEMICOLON, "Expected ';'");
            return fdecls(parent);
        } else if (in(FOLLOW, ll.type)) {
            //not needed
        } else {
            error(ll);
        }
        return parent;
    }

    private Tree fdec() {
        // <fdec> ::= def <type> <fname> ( <params> ) <declarations> <statement_seq> fed
        TokenType[] FIRST = {TokenType.DEF};
        TokenType[] FOLLOW = {TokenType.SEMICOLON};

        Tree root = new Tree("fdec");
        Token ll = lookAhead();
        if (in(FIRST, ll.type)) {
            match(TokenType.DEF, "Expected DEF");
            Tree typeTree = type();
            TokenType returnType = ((Leaf) typeTree.children.get(0)).token.type;
            String funcName = lookAhead().lexeme;
            fname();
            match(TokenType.OPAREN, "Expected '('");

            currFunc = funcSymbolTable.size() + 1;
            symbolTables.put(currFunc, new SymbolTable());
            ArrayList<TokenType> paramList = new ArrayList<>();
            ArrayList<String> nameList = new ArrayList<>();
            params(paramList, nameList);
            FunctionSymbol func = new FunctionSymbol(funcName, returnType, paramList, nameList, root);
            funcSymbolTable.add(func);

            root.symbolId = currFunc;

            match(TokenType.CPAREN, "Expected ')'");
            root.addChild(declarations(null));
            root.addChild(statement_seq(null));
            match(TokenType.FED, "Expected FED");
        } else {
            error(ll);
        }
        return root;
    }

    private Tree params(ArrayList<TokenType> typeList, ArrayList<String> nameList) {
        // <params> ::= <type> <var> <params_list> | ε
        TokenType[] FIRST = {TokenType.TYPE_INT, TokenType.TYPE_DOUBLE};
        TokenType[] FOLLOW = {TokenType.CPAREN};

        Tree root = new Tree("params");
        Token ll = lookAhead();

        if (in(FIRST, ll.type)) {
            // <type> <var> <params_list>
            typeList.add(ll.type);
            root.addChild(type());
            Token varToken = lookAhead();
            Tree var = var();
            root.addChild(var);
            TokenType symType = ll.type;
            nameList.add(varToken.lexeme);
            var.symbolId = symbolTables.get(currFunc).createSymbol(varToken.lexeme, symType, 0);
            root.addChild(params_list(typeList,nameList));


        } else if (in(FOLLOW, ll.type)) {
            root.addChild(epsilon());
        } else {
            error(ll);
        }
        return root;
    }

    private Tree params_list(ArrayList<TokenType> typeList, ArrayList<String> nameList) {
        // <params_list> ::= , <params> | ε
        TokenType[] FIRST = {TokenType.COMMA};
        TokenType[] FOLLOW = {TokenType.CPAREN};

        Tree root = new Tree("params_list");
        Token ll = lookAhead();
        if (in(FIRST, ll.type)) {
            match(TokenType.COMMA, "Expected ','");
            root.addChild(params(typeList, nameList));
        } else if (in(FOLLOW, ll.type)) {
            root.addChild(epsilon());
        } else {
            error(ll);
        }
        return root;
    }

    private Tree fname() {
        // <fname> ::= <id>
        TokenType[] FIRST = {TokenType.ID};
        TokenType[] FOLLOW = {TokenType.OPAREN};

        Tree root = new Tree("fname");
        Token ll = lookAhead();
        if (in(FIRST, ll.type)) {
            root.addChild(match(TokenType.ID, "Expected an identifier"));
        } else {
            error(ll);
        }
        return root;
    }

    private Tree declarations(Tree root) {
        // <declarations> ::= <decl>; <declarations> | ε
        TokenType[] FIRST = {TokenType.TYPE_INT, TokenType.TYPE_DOUBLE};
        TokenType[] FOLLOW = {TokenType.IF, TokenType.WHILE, TokenType.PRINT, TokenType.RETURN, TokenType.ID, TokenType.SEMICOLON};

        if (root == null) {
            root = new Tree("declarations");
        }
        Token ll = lookAhead();
        if (in(FIRST, ll.type)) {
            decl(root);
            match(TokenType.SEMICOLON, "Expected a semicolon");
            declarations(root);
        } else if (in(FOLLOW, ll.type)) {
            // all is well
        } else {
            error(ll);
        }
        return root;
    }

    private Tree decl(Tree root) {
        // <decl> := <type> <varlist>
        TokenType[] FIRST = {TokenType.TYPE_INT, TokenType.TYPE_DOUBLE};
        TokenType[] FOLLOW = {TokenType.SEMICOLON};

        Token ll = lookAhead();
        if (in(FIRST, ll.type)) {
            TokenType symType = ll.type;
            type();
            varlist(root, symType);
        } else {
            error(ll);
        }
        return root;
    }

    private Tree type() {
        // <type> := int | double
        TokenType[] INT_FIRST = {TokenType.TYPE_INT};
        TokenType[] DOUBLE_FIRST = {TokenType.TYPE_DOUBLE};
        TokenType[] FOLLOW = {TokenType.ID};

        Tree root = new Tree("type");
        Token ll = lookAhead();
        if (in(INT_FIRST, ll.type)) {
            root.addChild(match(TokenType.TYPE_INT, "Expected 'int' type"));
        } else if (in(DOUBLE_FIRST, ll.type)) {
            root.addChild(match(TokenType.TYPE_DOUBLE, "Expected 'double' type"));
        } else {
            error(ll);
        }
        return root;
    }

    private Tree varlist(Tree root, TokenType symType) {
        // <varlist> ::= <var><varlist’>
        TokenType[] FIRST = {TokenType.ID};
        TokenType[] FOLLOW = {TokenType.SEMICOLON};

        Token ll = lookAhead();
        if (in(FIRST, ll.type)) {
            Tree var = var();
            root.addChild(var);
            var.symbolId = symbolTables.get(currFunc).createSymbol(ll.lexeme, symType, 0);
            varlistPrime(root, symType);
        } else {
            error(ll);
        }
        return root;
    }

    private Tree varlistPrime(Tree root, TokenType symType) {
        // <varlist’> ::= , <varlist> | ε
        TokenType[] FIRST = {TokenType.COMMA};
        TokenType[] FOLLOW = {TokenType.SEMICOLON};

        Token ll = lookAhead();
        if (in(FIRST, ll.type)) {
            match(TokenType.COMMA, "Expected Comma");
            varlist(root, symType);
        } else if (in(FOLLOW, ll.type)) {
            // all is well
        } else {
            error(ll);
        }
        return root;
    }

    private Tree statement_seq(Tree parent) {
        // <statement_seq> ::= <statement><statement_seq’>
        TokenType[] FIRST = {TokenType.IF, TokenType.WHILE, TokenType.PRINT, TokenType.RETURN, TokenType.ID, TokenType.SEMICOLON};
        TokenType[] FOLLOW = {TokenType.PERIOD, TokenType.FI, TokenType.ELSE, TokenType.OD, TokenType.FED};
        if (parent == null) {
            parent = new Tree("statement_seq");
        }
        Token ll = lookAhead();
        if (in(FIRST, ll.type)) {
            parent.addChild(statement());
            statement_seqPrime(parent);
        } else if (in(FOLLOW, ll.type)) {
            //not needed
        } else {
            error(ll);
        }
        return parent;
    }

    private Tree statement_seqPrime(Tree parent) {
        // <statement_seq’> ::= ; <statement_seq>  | ε

        TokenType[] FIRST = {TokenType.SEMICOLON};
        TokenType[] FOLLOW = {TokenType.PERIOD, TokenType.FI, TokenType.ELSE, TokenType.OD, TokenType.FED};

        Tree root = null;
        Token ll = lookAhead();
        if (in(FIRST, ll.type)) {
            match(TokenType.SEMICOLON, "Expected ;");
            root = statement_seq(parent);
        } else if (in(FOLLOW, ll.type)) {
            return epsilon();
        } else {
            error(ll);
        }
        return root;
    }


    private Tree statement() {
//    <statement> ::= <var> = <expr> |
//                    if <bexpr> then <statement_seq> <ifbody>|
//                    while <bexpr> do <statement_seq> od |
//                    print <expr> |
//                    return <expr> | ε
        TokenType[] VAR_EXP_FIRST = {TokenType.ID};
        TokenType[] IF_FIRST = {TokenType.IF};
        TokenType[] WHILE_FIRST = {TokenType.WHILE};
        TokenType[] PRINT_FIRST = {TokenType.PRINT};
        TokenType[] RETURN_FIRST = {TokenType.RETURN};
        TokenType[] FOLLOW = {TokenType.SEMICOLON, TokenType.PERIOD, TokenType.FI, TokenType.ELSE, TokenType.OD};

        Tree root;
        Token ll = lookAhead();
        if (in(VAR_EXP_FIRST, ll.type)) {
            root = new Tree("=");
            Tree var = var();
            root.addChild(var);
            match(TokenType.ASSIGN, "Expected assignment");
            root.addChild(expr());
            return root;
        } else if (in(IF_FIRST, ll.type)) {
            root = new Tree("if");
            match(TokenType.IF, "Expected if");
            root.addChild(bexpr());
            match(TokenType.THEN, "Expected then");
            root.addChild(statement_seq(null));
            return ifbody(root);
        } else if (in(WHILE_FIRST, ll.type)) {
            root = new Tree("while");
            match(TokenType.WHILE, "Expected while");
            root.addChild(bexpr());
            match(TokenType.DO, "Expected DO");
            root.addChild(statement_seq(null));
            match(TokenType.OD, "Expected OD");
            return root;
        } else if (in(PRINT_FIRST, ll.type)) {
            root = new Tree("print");
            match(TokenType.PRINT, "expected print");
            root.addChild(expr());
            return root;
        } else if (in(RETURN_FIRST, ll.type)) {
            root = new Tree("return");
            match(TokenType.RETURN, "Expected return");
            root.addChild(expr());
            return root;
        } else if (in(FOLLOW, ll.type)) {
            return epsilon();
        } else {
            error(ll);
        }
        throw new IllegalStateException("Bad State");
    }

    private Tree ifbody(Tree ifHead) {
        // <ifbody> ::= fi | else <statement_seq> fi
        TokenType[] FI_FIRST = {TokenType.FI};
        TokenType[] ELSE_FIRST = {TokenType.ELSE};
        TokenType[] FOLLOW = {TokenType.PERIOD, TokenType.FI, TokenType.ELSE, TokenType.OD, TokenType.SEMICOLON};

        Token ll = lookAhead();
        if (in(FI_FIRST, ll.type)) {
            match(TokenType.FI, "Expected fi");
            return ifHead;
        } else if (in(ELSE_FIRST, ll.type)) {
            match(TokenType.ELSE, "");
            ifHead.addChild(statement_seq(null));
            match(TokenType.FI, "Expected fi");
            return ifHead;
        } else {
            error(ll);
        }

        throw new IllegalStateException("Bad State");
    }


    private Tree expr() {
        // <expr> ::= <term><expr’>
        TokenType[] FIRST = {TokenType.ID, TokenType.NUM, TokenType.OPAREN, TokenType.MINUS};
        TokenType[] FOLLOW = {TokenType.CPAREN, TokenType.COMMA, TokenType.SEMICOLON, TokenType.LT, TokenType.GT, TokenType.EQ, TokenType.LE, TokenType.GE, TokenType.NE, TokenType.CSPAREN, TokenType.FI, TokenType.ELSE, TokenType.OD, TokenType.PERIOD};

        Tree root = null;
        Token ll = lookAhead();
        if (in(FIRST, ll.type)) {
            Tree term = term();
            if (root == null) {
                return exprPrime(term);
            } else {
                root.addChild(exprPrime(term));
                return root;
            }
        } else {
            error(ll);
        }
        throw new IllegalStateException("Bad state");
    }


    private Tree exprPrime(Tree lhs) {
        // <expr’> ::= +<term><expr’> | -<term><expr’> | ε
        TokenType[] PLUS_FIRST = {TokenType.PLUS};
        TokenType[] MINUS_FIRST = {TokenType.MINUS};
        TokenType[] FOLLOW = {TokenType.CPAREN, TokenType.COMMA, TokenType.SEMICOLON, TokenType.LT, TokenType.GT, TokenType.EQ, TokenType.LE, TokenType.GE, TokenType.NE, TokenType.CSPAREN, TokenType.FI, TokenType.ELSE, TokenType.OD, TokenType.PERIOD, TokenType.FED};

        Tree root = new Tree(null);
        root.addChild(lhs);
        Token ll = lookAhead();
        if (in(PLUS_FIRST, ll.type)) {
            match(TokenType.PLUS, "expected +");
            root.op = "+";
            root.addChild(term());
            return exprPrime(root);
        } else if (in(MINUS_FIRST, ll.type)) {
            match(TokenType.MINUS, "expected -");
            root.op = "-";
            root.addChild(term());
            return exprPrime(root);
        } else if (in(FOLLOW, ll.type)) {
            return lhs;
        } else {
            error(ll);
        }
        throw new IllegalStateException("Bad state");
    }

    private Tree term() {
        // <term> ::= <factor><term’>
        TokenType[] FIRST = {TokenType.ID, TokenType.NUM, TokenType.OPAREN};
        TokenType[] FOLLOW = {TokenType.PLUS, TokenType.MINUS, TokenType.CPAREN, TokenType.COMMA, TokenType.SEMICOLON, TokenType.LT, TokenType.GT, TokenType.EQ, TokenType.LE, TokenType.GE, TokenType.NE, TokenType.CSPAREN, TokenType.FI, TokenType.ELSE, TokenType.OD, TokenType.PERIOD};

        Token ll = lookAhead();
        if (in(FIRST, ll.type)) {
            Tree lhs = factor();
            return termPrime(lhs);
        } else {
            error(ll);
        }
        throw new IllegalStateException("Bad state");
    }


    private Tree termPrime(Tree lhs) {
        // <term’> ::= *<factor><term’> | /<factor><term’> | %<factor><term’> | ε
        TokenType[] MUL_FIRST = {TokenType.MULT};
        TokenType[] DIV_FIRST = {TokenType.DIVIDE};
        TokenType[] MOD_FIRST = {TokenType.MOD};
        TokenType[] FOLLOW = {TokenType.PLUS, TokenType.MINUS, TokenType.CPAREN, TokenType.COMMA, TokenType.SEMICOLON, TokenType.LT, TokenType.GT, TokenType.EQ, TokenType.LE, TokenType.GE, TokenType.NE, TokenType.CSPAREN, TokenType.FI, TokenType.ELSE, TokenType.OD, TokenType.PERIOD, TokenType.FED};

        Tree root = new Tree(null);
        root.addChild(lhs);

        Token ll = lookAhead();
        if (in(MUL_FIRST, ll.type)) {
            root.op = "*";
            match(TokenType.MULT, "Expected *");
            root.addChild(factor());
            return termPrime(root);
        } else if (in(DIV_FIRST, ll.type)) {
            root.op = "/";
            match(TokenType.DIVIDE, "Expected /");
            root.addChild(factor());
            return termPrime(root);
        } else if (in(MOD_FIRST, ll.type)) {
            root.op = "%";
            match(TokenType.MOD, "Expected %");
            root.addChild(factor());
            return termPrime(root);
        } else if (in(FOLLOW, ll.type)) {
            return lhs;
        } else {
            error(ll);
        }
        throw new IllegalStateException("Bad state");
    }

    private Tree factor() {
        // <factor> ::= <var><factor'> | <number> | (<expr>)
        TokenType[] VAR_FIRST = {TokenType.ID};
        TokenType[] NUM_FIRST = {TokenType.NUM};
        TokenType[] EXPR_FIRST = {TokenType.OPAREN};
        TokenType[] FOLLOW = {TokenType.MULT, TokenType.DIVIDE, TokenType.MOD, TokenType.PLUS, TokenType.MINUS, TokenType.CPAREN, TokenType.COMMA, TokenType.SEMICOLON, TokenType.LT, TokenType.GT, TokenType.EQ, TokenType.LE, TokenType.GE, TokenType.NE, TokenType.CSPAREN, TokenType.FI, TokenType.ELSE, TokenType.OD, TokenType.PERIOD};

        Token ll = lookAhead();
        if (in(VAR_FIRST, ll.type)) {
            Tree var = var();
            return factorPrime(var);
        } else if (in(NUM_FIRST, ll.type)) {
            return match(TokenType.NUM, "");
        } else if (in(EXPR_FIRST, ll.type)) {
            match(TokenType.OPAREN, "");
            Tree expr = expr();
            match(TokenType.CPAREN, "");
            return expr;
        } else {
            error(ll);
        }
        throw new RuntimeException("Illegal state");
    }

    private Tree factorPrime(Tree functionName) {
        // <factor> ::= (<exprseq>) | ε
        TokenType[] FIRST = {TokenType.OPAREN};
        TokenType[] FOLLOW = {TokenType.MULT, TokenType.DIVIDE, TokenType.MOD, TokenType.PLUS, TokenType.MINUS, TokenType.CPAREN, TokenType.COMMA, TokenType.SEMICOLON, TokenType.LT, TokenType.GT, TokenType.EQ, TokenType.LE, TokenType.GE, TokenType.NE, TokenType.CSPAREN, TokenType.FI, TokenType.ELSE, TokenType.OD, TokenType.PERIOD, TokenType.FED};

        Token ll = lookAhead();
        if (in(FIRST, ll.type)) {
            match(TokenType.OPAREN, "");
            Tree fcall = new Tree("fcall");
            fcall.symbolId = getFuncSymbolId(functionName.op);

            fcall.addChild(functionName);

            fcall.addChild(exprseq(null));
            match(TokenType.CPAREN, "Missing )");
            return fcall;
        } else if (in(FOLLOW, ll.type)) {
            return functionName;
        } else {
            error(ll);
        }
        throw new IllegalStateException();
    }


    private Tree exprseq(Tree parent) {
        // <exprseq> ::= <expr><exprseq’> | ε
        TokenType[] FIRST = {TokenType.ID, TokenType.NUM, TokenType.OPAREN, TokenType.MINUS};
        TokenType[] FOLLOW = {TokenType.CPAREN};
        if (parent == null) {
            parent = new Tree("exprseq");
        }
        Token ll = lookAhead();
        if (in(FIRST, ll.type)) {
            parent.addChild(expr());
            exprseqPrime(parent);
        } else if (in(FOLLOW, ll.type)) {
            //not needed
        } else {
            error(ll);
        }
        return parent;
    }

    private Tree exprseqPrime(Tree parent) {
        // <exprseq’> ::= , <exprseq’> | ε
        TokenType[] FIRST = {TokenType.COMMA};
        TokenType[] FOLLOW = {TokenType.CPAREN};

        Tree root = null;
        Token ll = lookAhead();
        if (in(FIRST, ll.type)) {
            match(TokenType.COMMA, "");
            root = exprseq(parent);
        } else if (in(FOLLOW, ll.type)) {
            return epsilon();
        } else {
            error(ll);
        }
        return root;
    }

    private Tree bexpr() {
        // <bexpr> ::= <bterm><bexpr’>
        TokenType[] FIRST = {TokenType.OPAREN, TokenType.NOT};
        TokenType[] FOLLOW = {TokenType.CPAREN, TokenType.THEN, TokenType.DO};

        Tree root = null;
        Token ll = lookAhead();
        if (in(FIRST, ll.type)) {
            Tree lhs = bterm();
            root = bexprPrime(lhs);
        } else {
            error(ll);
        }
        return root;
    }

    private Tree bexprPrime(Tree lhs) {
        // <bexpr’> := or <bexpr> | ε
        TokenType[] FIRST = {TokenType.OR};
        TokenType[] FOLLOW = {TokenType.CPAREN, TokenType.THEN, TokenType.DO};

        Tree root = null;
        Token ll = lookAhead();
        if (in(FIRST, ll.type)) {
            match(TokenType.OR, "");
            root = new Tree("or");
            root.addChild(lhs);
            root.addChild(bexpr());
        } else if (in(FOLLOW, ll.type)) {
            return lhs;
        } else {
            error(ll);
        }
        return root;
    }

    private Tree bterm() {
        // <bterm> ::= <bfactor><bterm’>
        TokenType[] FIRST = {TokenType.OPAREN, TokenType.NOT};
        TokenType[] FOLLOW = {TokenType.CPAREN, TokenType.THEN, TokenType.DO};

        Tree root = null;
        Token ll = lookAhead();
        if (in(FIRST, ll.type)) {
            Tree lhs = bfactor();
            root = btermPrime(lhs);
        } else if (in(FOLLOW, ll.type)) {
            root.addChild(epsilon());
        } else {
            error(ll);
        }
        return root;
    }

    private Tree btermPrime(Tree lhs) {
        // <bterm’> ::= and <bfactor><bterm’> | ε
        TokenType[] FIRST = {TokenType.AND};
        TokenType[] FOLLOW = {TokenType.CPAREN, TokenType.THEN, TokenType.DO, TokenType.OR};

        Tree root = null;
        Token ll = lookAhead();
        if (in(FIRST, ll.type)) {
            match(TokenType.AND, "");
            root = new Tree("and");
            root.addChild(lhs);
            root.addChild(bfactor());
            return btermPrime(root);
        } else if (in(FOLLOW, ll.type)) {
            return lhs;
        } else {
            error(ll);
        }
        return root;
    }

    private Tree bfactor() {
        // <bfactor> ::= (<bexpr>) | not <bfactor> | (<expr> <comp> <expr>)
        TokenType[] BEXPR_FIRST = {TokenType.OPAREN};
        TokenType[] NOTBFACTOR_FIRST = {TokenType.NOT};
        TokenType[] EXPR_COMP_FIRST = {TokenType.OPAREN};
        TokenType[] FOLLOW = {TokenType.CPAREN, TokenType.THEN, TokenType.DO};

        Tree root = null;

        Token ll = lookAhead();
        if (in(EXPR_COMP_FIRST, ll.type)) {
            match(TokenType.OPAREN, "Expected '('");
            Tree lhs = expr();
            root = comp();
            root.addChild(lhs);
            root.addChild(expr());
            match(TokenType.CPAREN, "Expected ')'");
        } else if (in(BEXPR_FIRST, ll.type)) {
            // (<bexpr>)
            match(TokenType.OPAREN, "Expected '('");
            root.addChild(bexpr());
            match(TokenType.CPAREN, "Expected ')'");
        } else if (in(NOTBFACTOR_FIRST, ll.type)) {
            // not <bfactor>

            root.addChild(match(TokenType.NOT, "Expected 'not'"));
            root = new Tree("not");
            root.addChild(bfactor());
        } else {
            error(ll);
        }
        return root;
    }

    private Tree comp() {
        // <comp> ::= < | > | == | <= | >= | <>
        TokenType[] COMP = {TokenType.LT, TokenType.GT, TokenType.EQ, TokenType.LE, TokenType.GE, TokenType.NE};
        TokenType[] FOLLOW = {TokenType.ID, TokenType.NUM, TokenType.CPAREN};

        Tree root = null;
        Token ll = lookAhead();
        if (in(COMP, ll.type)) {
            root = new Tree(getNextToken().lexeme);
        } else {
            error(ll);
        }
        return root;
    }

    private Tree var() {
        // <var> ::= <id><arrayIndex> | ε
        TokenType[] FIRST = {TokenType.ID};
        TokenType[] FOLLOW = {TokenType.COMMA, TokenType.ASSIGN, TokenType.MULT, TokenType.DIVIDE, TokenType.MOD, TokenType.PLUS, TokenType.MINUS, TokenType.CPAREN, TokenType.SEMICOLON, TokenType.LT, TokenType.GT, TokenType.EQ, TokenType.LE, TokenType.GE, TokenType.NE, TokenType.CSPAREN, TokenType.FED, TokenType.OPAREN};

        Token ll = lookAhead();
        if (in(FIRST, ll.type)) {
            Tree id = match(TokenType.ID, "");
            Tree arrIndex = arrayIndex(id);
            int symId = symbolTables.get(currFunc).findSymbol(id.op);
            if (symId >= 0) {
                id.symbolId = symId;
            }
            return arrIndex;
        } else if (in(FOLLOW, ll.type)) {
            // all is well
            return null;
        } else {
            error(ll);
        }
        throw new IllegalStateException();
    }

    private Tree arrayIndex(Tree lhs) {
        // <arrayIndex> ::= [<expr>] | ε
        TokenType[] FIRST = {TokenType.OSPAREN};
        TokenType[] FOLLOW = {TokenType.COMMA, TokenType.ASSIGN, TokenType.MULT, TokenType.DIVIDE, TokenType.MOD, TokenType.PLUS, TokenType.MINUS, TokenType.OPAREN, TokenType.CPAREN, TokenType.SEMICOLON, TokenType.LT, TokenType.GT, TokenType.EQ, TokenType.LE, TokenType.GE, TokenType.NE, TokenType.CSPAREN, TokenType.FED, TokenType.OPAREN};

        Tree root = new Tree("arrayIndex");
        Token ll = lookAhead();
        if (in(FIRST, ll.type)) {
            match(TokenType.OSPAREN, "");
            root.addChild(lhs);
            root.addChild(expr());
            match(TokenType.CSPAREN, "Expected ']'");
        } else if (in(FOLLOW, ll.type)) {
            return lhs;
        } else {
            error(ll);
        }
        return root;
    }

    private Tree program() {
        Tree program = new Tree("program");
        program.addChild(fdecls(null));
        currFunc = 0;
        program.addChild(declarations(null));
        program.addChild(statement_seq(null));
        match(TokenType.PERIOD, "Expected period");
        return program;
    }

    Tree parse() {
        ast = program();
        return ast;
    }

    private Tree match(TokenType type, String errMsg) {
        Token t = getNextToken();
        if (t.type != type) {
            error(t);
        }
        return new Leaf(t);
    }

    private boolean in(TokenType[] types, TokenType type) {
        return Arrays.asList(types).contains(type);
    }

    void error(Token t) {
        String msg = "Didn't expect '" + t.toString() + "'";
        throw new RuntimeException(msg);
    }

}



class Compiler {
    Parser parser;
    InstructionFactory factory;
    ConstantPoolGen cp;
    ClassGen cg;

    Compiler(Parser parser) {
        this.parser = parser;
        this.cg = new ClassGen("gen_class", "java.lang.Object", "gen_class.java", ACC_PUBLIC | ACC_SUPER, new String[] {  });
        this.cp = cg.getConstantPool();
        this.factory = new InstructionFactory(cg,cp);
    }


    private void createMethod(Tree fdec){
        InstructionList il = new InstructionList();
        FunctionSymbol fsymbol = parser.funcSymbolTable.get(fdec.symbolId);

        Type[] paramTypes = new Type[fsymbol.paramTypes.size()];

        for(int i=0; i>= fsymbol.paramTypes.size(); i++){
            paramTypes[i] = mapType(fsymbol.paramTypes.get(i));
        }

        MethodGen method = new MethodGen(ACC_PUBLIC | ACC_STATIC, mapType(fsymbol.returnType), paramTypes, (String[]) fsymbol.nameList.toArray(), fsymbol.fname, "gen_class", il, cp);

        compileStatementSeq(fdec.children.get(1), il, parser.symbolTables.get(fdec.symbolId));


        cg.addMethod(method.getMethod());
        il.dispose();
    }

    private void compileProgram(Tree program) {
        assert program.op == "program";

        for(Tree fdec: program.children.get(0).children) {
            createMethod(fdec);
        }

        //compileStatementSeq(program.children.get(2), il, parser.symbolTables.get(fdec.symbolId));

    }

    public static Type mapType(TokenType tt){
        switch(tt){
            case TYPE_DOUBLE:
                return Type.DOUBLE;
            case TYPE_INT:
                return Type.INT;
            case

        }

    }



    /**
     * @param statementSeq
     * @return True if returning from function; False if continuing in function
     */
    private boolean compileStatementSeq(Tree statementSeq, InstructionList il, SymbolTable symtable) {

        assert statementSeq.op == "statement_seq";


        for (Tree child : statementSeq.children) {
            switch (child.op) {
                case "=":
                    compileAssign(child, il, symtable);
                    break;
                case "if":
                    if (compileIf(child, il, symtable)) {
                        return true;
                    }
                    break;
                case "while":
                    if (compileWhile(child, il, symtable)) {
                        return true;
                    }
                    break;
                case "print":
                    compilePrint(child, il, symtable);
                    break;
                case "return":
                    compileReturn(child, il, symtable);
                    return true;
            }
        }
        return false;
    }

    private void evalReturn(Tree returnNode) {
        Object result = evalExpr(returnNode.children.get(0));
        stack.peek().returnValue = result;
    }

    private void evalPrint(Tree print) {
        Object result = evalExpr(print.children.get(0));
        System.out.println("// " + result);
    }

    private boolean evalWhile(Tree whileNode) {
        Tree bexpr = whileNode.children.get(0);

        while (evalBexpr(bexpr)) {
            if (evalStatementSeq(whileNode.children.get(1))) {
                return true;
            }
        }
        return false;
    }

    private Double toDouble(Object o) {
        if (o instanceof Double) {
            return (Double) o;
        } else {
            return ((Integer) o).doubleValue();
        }
    }

    private boolean evalBexpr(Tree bexpr) {
        double lhs, rhs;
        boolean blhs, brhs;
        switch (bexpr.op) {
            case "==":
                lhs = toDouble(evalExpr(bexpr.children.get(0)));
                rhs = toDouble(evalExpr(bexpr.children.get(1)));
                return lhs == rhs;
            case "<>":
                lhs = toDouble(evalExpr(bexpr.children.get(0)));
                rhs = toDouble(evalExpr(bexpr.children.get(1)));
                return lhs != rhs;
            case "<":
                lhs = toDouble(evalExpr(bexpr.children.get(0)));
                rhs = toDouble(evalExpr(bexpr.children.get(1)));
                return lhs < rhs;
            case ">":
                lhs = toDouble(evalExpr(bexpr.children.get(0)));
                rhs = toDouble(evalExpr(bexpr.children.get(1)));
                return lhs > rhs;
            case "<=":
                lhs = toDouble(evalExpr(bexpr.children.get(0)));
                rhs = toDouble(evalExpr(bexpr.children.get(1)));
                return lhs <= rhs;
            case ">=":
                lhs = toDouble(evalExpr(bexpr.children.get(0)));
                rhs = toDouble(evalExpr(bexpr.children.get(1)));
                return lhs >= rhs;
            case "or":
                blhs = evalBexpr(bexpr.children.get(0));
                brhs = evalBexpr(bexpr.children.get(1));
                return blhs || brhs;
            case "and":
                blhs = evalBexpr(bexpr.children.get(0));
                brhs = evalBexpr(bexpr.children.get(1));
                return blhs && brhs;
            case "not":
                blhs = evalBexpr(bexpr.children.get(0));
                return !blhs;
        }
        throw new IllegalStateException();
    }

    private void compileAssign(Tree assignNode, InstructionList il, SymbolTable symtable) {
        Tree lhs = assignNode.children.get(0);
        if (lhs.op.equals("arrayIndex")) {
            throw new IllegalStateException("We don't support array indexing");
        }

        Tree rhs = assignNode.children.get(1);
        compileExpr(rhs, il, symtable);

        assert lhs.symbolId != null;
        Symbol sym = symtable.symbols.get(lhs.symbolId);
        il.append(factory.createStore(mapType(sym.symbolType), lhs.symbolId));
    }

    private TokenType compileExpr(Tree expr, InstructionList il, SymbolTable symtable) {
        TokenType lhs;
        TokenType rhs;
        switch (expr.op) {
            case "+":
                lhs = compileExpr(expr.children.get(0), il, symtable);
                rhs = compileExpr(expr.children.get(1), il, symtable);



                if (lhs == TokenType.TYPE_INT && rhs == TokenType.TYPE_INT) {

                    il.append(InstructionConst.IADD);
                    return TokenType.TYPE_INT;
                } else {
                    Double lDouble = toDouble(lhs);
                    Double rDouble = toDouble(rhs);

                    il.append(InstructionConst.DADD);
                }
            case "-":
                lhs = compileExpr(expr.children.get(0), il, symtable);
                rhs = compileExpr(expr.children.get(1), il, symtable);

                if (lhs instanceof Integer && rhs instanceof Integer) {
                    Integer lInt = (Integer) lhs;
                    Integer rInt = (Integer) rhs;
                    return lInt - rInt;
                } else {
                    Double lDouble = (Double) lhs;
                    Double rDouble = (Double) rhs;
                    return lDouble - rDouble;
                }
            case "*":
                lhs = compileExpr(expr.children.get(0), il, symtable);
                rhs = compileExpr(expr.children.get(1), il, symtable);

                if (lhs instanceof Integer && rhs instanceof Integer) {
                    Integer lInt = (Integer) lhs;
                    Integer rInt = (Integer) rhs;
                    return lInt * rInt;
                } else {
                    Double lDouble = (Double) lhs;
                    Double rDouble = (Double) rhs;
                    return lDouble * rDouble;
                }
            case "/":
                lhs = compileExpr(expr.children.get(0), il, symtable);
                rhs = compileExpr(expr.children.get(1), il, symtable);

                if (lhs instanceof Integer && rhs instanceof Integer) {
                    Integer lInt = (Integer) lhs;
                    Integer rInt = (Integer) rhs;
                    return lInt / rInt;
                } else {
                    Double lDouble = (Double) lhs;
                    Double rDouble = (Double) rhs;
                    return lDouble / rDouble;
                }
            case "%":
                lhs = compileExpr(expr.children.get(0), il, symtable);
                rhs = compileExpr(expr.children.get(1), il, symtable);

                if (lhs instanceof Integer && rhs instanceof Integer) {
                    Integer lInt = (Integer) lhs;
                    Integer rInt = (Integer) rhs;
                    return lInt % rInt;
                } else {
                    throw new IllegalStateException("Can't modulo with doubles");
                }
            case "fcall":
                return compileFcall(expr);
            default:
                if (expr.symbolId != null) {
                    // it's a var
                    Object value = stack.peek().symbolTable.getValue(expr.symbolId);
                    return value;
                } else {
                    // it's a constant
                    try {
                        return Integer.parseInt(expr.op);
                    } catch (Exception e) {
                        return Double.parseDouble(expr.op);
                    }
                }
        }
    }

    private boolean compileIf(Tree ifNode) {
        boolean result = false;
        Tree condition = ifNode.children.get(0);
        if (evalBexpr(condition)) {
            result = compileStatementSeq(ifNode.children.get(1));
        } else if (ifNode.children.size() == 3) {
            result = compileStatementSeq(ifNode.children.get(2));
        }
        return result;
    }

    private Object compileFcall(Tree funcCall){

        SymbolTable symTable = parser.symbolTables.get(funcCall.symbolId).createBlankSymbolTable();
        FunctionSymbol fSymbol = parser.funcSymbolTable.get(funcCall.symbolId-1);
        StackFrame stackFrame = new StackFrame(symTable);

        evalExprseq(funcCall.children.get(1), stackFrame);
        stack.push(stackFrame);
        compileStatementSeq(fSymbol.fdec.children.get(1));
        Object result= stackFrame.returnValue;
        stack.pop();
        return result;

    }

    private void compileExprseq(Tree exprSeq, StackFrame stackFrame){
        for (int i=0; i < exprSeq.children.size() ; i++ ){
            Object result = compileExpr(exprSeq.children.get(i));
            stackFrame.symbolTable.setValue(i, result);

        }

    }
}

class StackFrame {
    SymbolTable symbolTable;
    Object returnValue;

    StackFrame(SymbolTable symTable) {
        this.symbolTable = symTable;
    }
}

class SymbolTable {
    ArrayList<Symbol> symbols = new ArrayList<Symbol>();

    SymbolTable() {
    }

    Object getValue(int id) {
        return symbols.get(id).value;
    }

    void setValue(int id, Object value) {
        symbols.get(id).value = value;
    }

    int createSymbol(String name, TokenType type, Object value) {
        Symbol sym = new Symbol(name, type, value);
        symbols.add(sym);
        return symbols.size() - 1;
    }

    SymbolTable createBlankSymbolTable() {
        SymbolTable symbolTable = new SymbolTable();

        for (Symbol sym : symbols) {
            symbolTable.createSymbol(sym.symbol, sym.symbolType, 0);
        }
        return symbolTable;
    }

    public void pprint() {
        for (int i = 0; i < symbols.size(); i++) {
            System.out.println("//\t" + i + "\t|\t" + symbols.get(i).toString());
        }
    }

    public int findSymbol(String symbolName) {
        for (int i = 0; i < symbols.size(); i++) {
            Symbol symbol = symbols.get(i);
            if (symbol.symbol.equals(symbolName)) {
                return i;
            }
        }
        return -1;
    }
}


class Interpreter {
    Parser parser;
    Stack<StackFrame> stack = new Stack<>();

    Interpreter(Parser parser) {
        this.parser = parser;
    }

    private void evalProgram(Tree program) {
        assert program.op == "program";
        evalStatementSeq(program.children.get(2));

    }

    /**
     * @param statementSeq
     * @return True if returning from function; False if continuing in function
     */
    private boolean evalStatementSeq(Tree statementSeq) {

        assert statementSeq.op == "statement_seq";


        for (Tree child : statementSeq.children) {
            switch (child.op) {
                case "=":
                    evalAssign(child);
                    break;
                case "if":
                    if (evalIf(child)) {
                        return true;
                    }
                    break;
                case "while":
                    if (evalWhile(child)) {
                        return true;
                    }
                    break;
                case "print":
                    evalPrint(child);
                    break;
                case "return":
                    evalReturn(child);
                    return true;
            }
        }
        return false;
    }

    private void evalReturn(Tree returnNode) {
        Object result = evalExpr(returnNode.children.get(0));
        stack.peek().returnValue = result;
    }

    private void evalPrint(Tree print) {
        Object result = evalExpr(print.children.get(0));
        System.out.println("// " + result);
    }

    private boolean evalWhile(Tree whileNode) {
        Tree bexpr = whileNode.children.get(0);

        while (evalBexpr(bexpr)) {
            if (evalStatementSeq(whileNode.children.get(1))) {
                return true;
            }
        }
        return false;
    }

    private Double toDouble(Object o) {
        if (o instanceof Double) {
            return (Double) o;
        } else {
            return ((Integer) o).doubleValue();
        }
    }

    private boolean evalBexpr(Tree bexpr) {
        double lhs, rhs;
        boolean blhs, brhs;
        switch (bexpr.op) {
            case "==":
                lhs = toDouble(evalExpr(bexpr.children.get(0)));
                rhs = toDouble(evalExpr(bexpr.children.get(1)));
                return lhs == rhs;
            case "<>":
                lhs = toDouble(evalExpr(bexpr.children.get(0)));
                rhs = toDouble(evalExpr(bexpr.children.get(1)));
                return lhs != rhs;
            case "<":
                lhs = toDouble(evalExpr(bexpr.children.get(0)));
                rhs = toDouble(evalExpr(bexpr.children.get(1)));
                return lhs < rhs;
            case ">":
                lhs = toDouble(evalExpr(bexpr.children.get(0)));
                rhs = toDouble(evalExpr(bexpr.children.get(1)));
                return lhs > rhs;
            case "<=":
                lhs = toDouble(evalExpr(bexpr.children.get(0)));
                rhs = toDouble(evalExpr(bexpr.children.get(1)));
                return lhs <= rhs;
            case ">=":
                lhs = toDouble(evalExpr(bexpr.children.get(0)));
                rhs = toDouble(evalExpr(bexpr.children.get(1)));
                return lhs >= rhs;
            case "or":
                blhs = evalBexpr(bexpr.children.get(0));
                brhs = evalBexpr(bexpr.children.get(1));
                return blhs || brhs;
            case "and":
                blhs = evalBexpr(bexpr.children.get(0));
                brhs = evalBexpr(bexpr.children.get(1));
                return blhs && brhs;
            case "not":
                blhs = evalBexpr(bexpr.children.get(0));
                return !blhs;
        }
        throw new IllegalStateException();
    }

    private void evalAssign(Tree assignNode) {
        Tree lhs = assignNode.children.get(0);
        if (lhs.op.equals("arrayIndex")) {
            throw new IllegalStateException("We don't support array indexing");
        }

        Tree rhs = assignNode.children.get(1);
        Object result = evalExpr(rhs);
        StackFrame stackFrame = stack.peek();

        assert lhs.symbolId != null;
        stackFrame.symbolTable.setValue(lhs.symbolId, result);
    }

    private Object evalExpr(Tree expr) {
        Object lhs;
        Object rhs;
        switch (expr.op) {
            case "+":
                lhs = evalExpr(expr.children.get(0));
                rhs = evalExpr(expr.children.get(1));

                if (lhs instanceof Integer && rhs instanceof Integer) {
                    Integer lInt = (Integer) lhs;
                    Integer rInt = (Integer) rhs;
                    return lInt + rInt;
                } else {
                    Double lDouble = toDouble(lhs);
                    Double rDouble = toDouble(rhs);
                    return lDouble + rDouble;
                }
            case "-":
                lhs = evalExpr(expr.children.get(0));
                rhs = evalExpr(expr.children.get(1));

                if (lhs instanceof Integer && rhs instanceof Integer) {
                    Integer lInt = (Integer) lhs;
                    Integer rInt = (Integer) rhs;
                    return lInt - rInt;
                } else {
                    Double lDouble = (Double) lhs;
                    Double rDouble = (Double) rhs;
                    return lDouble - rDouble;
                }
            case "*":
                lhs = evalExpr(expr.children.get(0));
                rhs = evalExpr(expr.children.get(1));

                if (lhs instanceof Integer && rhs instanceof Integer) {
                    Integer lInt = (Integer) lhs;
                    Integer rInt = (Integer) rhs;
                    return lInt * rInt;
                } else {
                    Double lDouble = (Double) lhs;
                    Double rDouble = (Double) rhs;
                    return lDouble * rDouble;
                }
            case "/":
                lhs = evalExpr(expr.children.get(0));
                rhs = evalExpr(expr.children.get(1));

                if (lhs instanceof Integer && rhs instanceof Integer) {
                    Integer lInt = (Integer) lhs;
                    Integer rInt = (Integer) rhs;
                    return lInt / rInt;
                } else {
                    Double lDouble = (Double) lhs;
                    Double rDouble = (Double) rhs;
                    return lDouble / rDouble;
                }
            case "%":
                lhs = evalExpr(expr.children.get(0));
                rhs = evalExpr(expr.children.get(1));

                if (lhs instanceof Integer && rhs instanceof Integer) {
                    Integer lInt = (Integer) lhs;
                    Integer rInt = (Integer) rhs;
                    return lInt % rInt;
                } else {
                    throw new IllegalStateException("Can't modulo with doubles");
                }
            case "fcall":
                return evalFcall(expr);
            default:
                if (expr.symbolId != null) {
                    // it's a var
                    Object value = stack.peek().symbolTable.getValue(expr.symbolId);
                    return value;
                } else {
                    // it's a constant
                    try {
                        return Integer.parseInt(expr.op);
                    } catch (Exception e) {
                        return Double.parseDouble(expr.op);
                    }
                }
        }
    }

    private boolean evalIf(Tree ifNode) {
        boolean result = false;
        Tree condition = ifNode.children.get(0);
        if (evalBexpr(condition)) {
            result = evalStatementSeq(ifNode.children.get(1));
        } else if (ifNode.children.size() == 3) {
            result = evalStatementSeq(ifNode.children.get(2));
        }
        return result;
    }

    private Object evalFcall(Tree funcCall){

        SymbolTable symTable = parser.symbolTables.get(funcCall.symbolId).createBlankSymbolTable();
        FunctionSymbol fSymbol = parser.funcSymbolTable.get(funcCall.symbolId-1);
        StackFrame stackFrame = new StackFrame(symTable);

        evalExprseq(funcCall.children.get(1), stackFrame);
        stack.push(stackFrame);
        evalStatementSeq(fSymbol.fdec.children.get(1));
        Object result= stackFrame.returnValue;
        stack.pop();
        return result;

    }

    private void evalExprseq(Tree exprSeq, StackFrame stackFrame){
        for (int i=0; i < exprSeq.children.size() ; i++ ){
            Object result = evalExpr(exprSeq.children.get(i));
            stackFrame.symbolTable.setValue(i, result);

        }
    }

    public Type propagateTypes(Tree root) {

        Type lhs;
        Type rhs;
        switch (root.op) {
            case "+":
            case "-":
            case "*":
            case "/":
                lhs = propagateTypes(root.children.get(0));
                rhs = propagateTypes(root.children.get(1));
                if (lhs == Type.INT && rhs == Type.INT) {
                    root.type = Type.INT;
                    return Type.INT;
                }
                root.type = Type.DOUBLE;
                return Type.DOUBLE;
            case "%":
                lhs = propagateTypes(root.children.get(0));
                rhs = propagateTypes(root.children.get(1));

                if (lhs != Type.INT || rhs != Type.INT) {
                    throw new IllegalStateException("Can not modulo with doubles");
                }
                return Type.INT;
            case "fcall":
                return Compiler.mapType(parser.funcSymbolTable.get(root.symbolId).returnType);
            default:
                if (root.symbolId != null) {
                    // it's a var
                    TokenType type = stack.peek().symbolTable.symbols.get(root.symbolId).symbolType;
                    return Compiler.mapType(type);
                } else {
                    // it's a constant
                    try {
                        Integer.parseInt(root.op);
                        return Type.INT;
                    } catch (Exception e) {
                        try {
                            Double.parseDouble(root.op);
                            return Type.DOUBLE;
                        } catch {
                            for (Tree child: root.children) {
                                propagateTypes(child);
                            }
                            root.type = Type.VOID;
                            return Type.VOID;
                        }
                    }
                }
        }
    }

    public void evaluate() {
        Tree ast = parser.parse();

        propagateTypes(ast);

        SymbolTable mainSymTable = parser.symbolTables.get(0);
        StackFrame mainStackFrame = new StackFrame(mainSymTable);
        stack.push(mainStackFrame);

        System.out.println("PROGRAM OUTPUT:");
        evalProgram(ast);
    }
}

class StackFrame {
    SymbolTable symbolTable;
    Object returnValue;

    StackFrame(SymbolTable symTable) {
        this.symbolTable = symTable;
    }
}

class SymbolTable {
    ArrayList<Symbol> symbols = new ArrayList<Symbol>();

    SymbolTable() {
    }

    Object getValue(int id) {
        return symbols.get(id).value;
    }

    void setValue(int id, Object value) {
        symbols.get(id).value = value;
    }

    int createSymbol(String name, TokenType type, Object value) {
        Symbol sym = new Symbol(name, type, value);
        symbols.add(sym);
        return symbols.size() - 1;
    }

    SymbolTable createBlankSymbolTable() {
        SymbolTable symbolTable = new SymbolTable();

        for (Symbol sym : symbols) {
            symbolTable.createSymbol(sym.symbol, sym.symbolType, 0);
        }
        return symbolTable;
    }

    public void pprint() {
        for (int i = 0; i < symbols.size(); i++) {
            System.out.println("//\t" + i + "\t|\t" + symbols.get(i).toString());
        }
    }

    public int findSymbol(String symbolName) {
        for (int i = 0; i < symbols.size(); i++) {
            Symbol symbol = symbols.get(i);
            if (symbol.symbol.equals(symbolName)) {
                return i;
            }
        }
        return -1;
    }
}

class FunctionSymbol {
    String fname;
    TokenType returnType;
    ArrayList<TokenType> paramTypes;
    ArrayList<String> nameList;
    Tree fdec;

    FunctionSymbol(String fname, TokenType returnType, ArrayList<TokenType> paramTypes, ArrayList<String> nameList, Tree fdec) {
        this.fname = fname;
        this.returnType = returnType;
        this.paramTypes = paramTypes;
        this.nameList = nameList;
        this.fdec = fdec;
    }

    public String toString() {
        return (fname + " :: " + paramTypes + " -> " + returnType);
    }

}

class Symbol {
    String symbol;
    TokenType symbolType;
    Object value;


    Symbol(String symbol, TokenType symbolType, Object value) {
        this.symbol = symbol;
        this.symbolType = symbolType;
        this.value = value;
    }

    public String toString() {
        return (symbol + " :: " + symbolType + " := " + value);
    }

}

class A2 {
    static int dotRecurse(Tree tree, int count) {
        int newCount = count;

        String parentNode = "a" + count;
        System.out.println("  " + parentNode + " [label=\"" + tree.op + ": " + tree.symbolId + "\"]");
        for (Tree c : tree.children) {
            if (c instanceof Leaf) {
                System.out.println("  node [shape=underline];");
            } else {
                System.out.println("  node [shape=ellipse];");
            }
            newCount += 1;
            String childNode = "a" + newCount;
            System.out.println("  " + parentNode + " -> " + childNode + ";");
            newCount = dotRecurse(c, newCount);
        }
        return newCount;
    }

    static void dotGraph(Tree tree) {
        System.out.println("");
        System.out.println("digraph AST {");
        dotRecurse(tree, 0);
        System.out.println("}");
        System.out.print("//");
    }

    static void dumpSymbolTable(Parser parser) {
        System.out.println("// SYMBOL TABLE");
        for (HashMap.Entry<Integer, SymbolTable> entry : parser.symbolTables.entrySet()) {
            System.out.println("// SCOPE:" + entry.getKey());
            SymbolTable table = entry.getValue();
            table.pprint();
        }

        System.out.println("// Function Table:");
        for (int i = 0; i < parser.funcSymbolTable.size(); i++) {
            FunctionSymbol functionSymbol = parser.funcSymbolTable.get(i);
            System.out.println("//\t" + (i + 1) + "\t|\t" + functionSymbol.toString());
        }
    }

    public static void main(String[] args) {
        //try {
        Parser parser = new Parser();
        Interpreter interpreter = new Interpreter(parser);
        System.out.print("// ");
        interpreter.evaluate();
        if (args.length > 0) {
            dotGraph(parser.ast);
        }
        //} catch (Exception e) {
        //  System.out.println("\n//There was an error parsing:\n//  " + e.getMessage());
        //}
    }
}
