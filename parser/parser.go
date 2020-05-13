package parser

import (
	"fmt"
	"monkey-lang/ast"
	"monkey-lang/lexer"
	"monkey-lang/token"
	"strconv"
)

const (
	// iota assigns the constants to auto increasing numbers
	_ int = iota
	LOWEST
	EQUALS      // =
	LESSGREATER // < or >
	SUM         // +
	PRODUCT     // -
	PREFIX      // -x or !x
	CALL        // func(x)
)

var precedences = map[token.TokenType]int{
	token.EQ:       EQUALS,
	token.NOT_EQ:   EQUALS,
	token.LT:       LESSGREATER,
	token.GT:       LESSGREATER,
	token.PLUS:     SUM,
	token.MINUS:    SUM,
	token.ASTERISK: PRODUCT,
	token.SLASH:    PRODUCT,
}

type Parser struct {
	l *lexer.Lexer

	curToken  token.Token
	peekToken token.Token

	errors []string

	// map of functions with key tokentype and value fn
	prefixParseFns map[token.TokenType]prefixParseFn
	infixParseFns  map[token.TokenType]infixParseFn
}

type (
	// prefix parse function returns returns the right side of the prefix as an
	// expression
	prefixParseFn func() ast.Expression

	// infix parse function takes in the left side of infix and returns the right
	// side of the infix as an expression
	infixParseFn func(ast.Expression) ast.Expression
)

func New(l *lexer.Lexer) *Parser {
	p := &Parser{
		l:      l,
		errors: []string{},
	}

	// read two tokens, so that curToken and peekToken are both set
	p.nextToken()
	p.nextToken()

	// create map of <token, fn> for prefix fns and populate it with respective
	// token, fn pairs
	p.prefixParseFns = make(map[token.TokenType]prefixParseFn)
	p.registerPrefix(token.IDENT, p.parseIdentifier)
	p.registerPrefix(token.INT, p.parseIntegerLiteral)
	p.registerPrefix(token.MINUS, p.parsePrefixExpression)
	p.registerPrefix(token.BANG, p.parsePrefixExpression)

	// create map of <token, fn> for infix fns and populate with respective
	// token, fn pairs
	p.infixParseFns = make(map[token.TokenType]infixParseFn)
	p.registerInfix(token.PLUS, p.parseInfixExpression)
	p.registerInfix(token.MINUS, p.parseInfixExpression)
	p.registerInfix(token.SLASH, p.parseInfixExpression)
	p.registerInfix(token.ASTERISK, p.parseInfixExpression)
	p.registerInfix(token.EQ, p.parseInfixExpression)
	p.registerInfix(token.NOT_EQ, p.parseInfixExpression)
	p.registerInfix(token.LT, p.parseInfixExpression)
	p.registerInfix(token.GT, p.parseInfixExpression)

	return p
}

// accessor method for errors
func (p *Parser) Errors() []string {
	return p.errors
}

// setter method for prefix functions
func (p *Parser) registerPrefix(tokenType token.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

// setter method for infix functions
func (p *Parser) registerInfix(tokenType token.TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

// get precedence int for the next peek token
func (p *Parser) peekPrecedence() int {
	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}

	// if token is not found, return the lowest value as precedence
	return LOWEST
}

// get predcedence int for the current token
func (p *Parser) curPrecedence() int {
	if p, ok := precedences[p.curToken.Type]; ok {
		return p
	}

	// if token is not found, return the lowest value as precedence
	return LOWEST
}

// method to create error when next token is not what is expected
// creates error msg and adds to parser errors array
func (p *Parser) peekError(t token.TokenType) {
	msg := fmt.Sprintf("expected next token to be %s, got %s instead", t, p.peekToken.Type)
	p.errors = append(p.errors, msg)
}

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

// returns if the current token is of the expected type
func (p *Parser) curTokenIs(t token.TokenType) bool {
	return p.curToken.Type == t
}

// returns if the next token is of the expected type
func (p *Parser) peekTokenIs(t token.TokenType) bool {
	return p.peekToken.Type == t
}

// an assertion function that only advances the tokens if the type of the next
// token is expected
func (p *Parser) expectPeek(t token.TokenType) bool {
	if p.peekTokenIs(t) {
		p.nextToken()
		return true
	} else {
		p.peekError(t)
		return false
	}
}

// parses the program to return an array of parsed statements (done by
// ParseStatement)
func (p *Parser) ParseProgram() *ast.Program {
	// initalize program with empty structs
	// note: we can use pointer because go automatically dereferences them
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	// keep parsing until reaching the end of token, while incrementally adding
	// the statements into the Statements array
	for !p.curTokenIs(token.EOF) {
		stmt := p.parseStatement()
		// ensure Statement is valid
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		p.nextToken()
	}
	return program
}

// parses a statement based on the identifier present at the current token
// supports:
// - let statements
// - return statements
func (p *Parser) parseStatement() ast.Statement {
	// decide what type of statement is being parsing based on the first token
	// that is seen
	switch p.curToken.Type {
	case token.LET:
		return p.parseLetStatement()
	case token.RETURN:
		return p.parseReturnStatement()
	default:
		return p.parseExpressionStatement()
	}
}

// parses lets statements. statements are expected to have the form
// <let> <identifier> <assign> <expr> <semicolon>
// returns a LetStatement with Token, Name and Value
func (p *Parser) parseLetStatement() *ast.LetStatement {
	// initalize a let statement (current token is at let)
	stmt := &ast.LetStatement{Token: p.curToken}

	// next token should be the identifier, only advance token if so
	// else return statement as invalid (nil)
	if !p.expectPeek(token.IDENT) {
		return nil
	}

	// record the identifier into the statement
	stmt.Name = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	// next token should be =, only advance token if so
	// else return statement as invalid (nil)
	if !p.expectPeek(token.ASSIGN) {
		return nil
	}

	// TODO: skip till end of sentence
	for !p.curTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

// parses return statements. statements are expected to have the form
// <return> <expr> <semicoln>
// returns a ReturnStatement with Token and ReturnValue
func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	// initalize a return statement (current token is at return)
	stmt := &ast.ReturnStatement{Token: p.curToken}

	p.nextToken()

	// TODO: skip till end of sentence
	for !p.curTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

// parses expression statements.
// returns a ExpressionStatement with Token and Expression
func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	// initalize a expression statement (current token is at start of expr)
	stmt := &ast.ExpressionStatement{Token: p.curToken}

	stmt.Expression = p.parseExpression(LOWEST)

	// a semicolon is optional: if it is encountered, call advance tokens.
	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

// parse expression using map of <token, fn> defined by registerPrefix
// parse fns do not advance tokens, they start with curToken being type of
// associated token and end with curToken being the last token that is part of
// the expression type
// takes in a precedence argument, parses expression until it reaches a
// precedence that is lower
func (p *Parser) parseExpression(precedence int) ast.Expression {
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		// not all token types will have the corresponding fn in the fns map, so
		// display error if encounter something that we don't know how to parse
		msg := fmt.Sprintf("no prefix parse function for %s found", p.curToken.Type)
		p.errors = append(p.errors, msg)
		return nil
	}
	leftExp := prefix()
	for !p.peekTokenIs(token.SEMICOLON) && precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp
		}

		p.nextToken()

		leftExp = infix(leftExp)
	}

	return leftExp
}

func (p *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseIntegerLiteral() ast.Expression {
	lit := &ast.IntegerLiteral{Token: p.curToken}

	// parse string to int64
	// if it works save the value under IntegerLiteral
	// else, return error to parser
	value, err := strconv.ParseInt(p.curToken.Literal, 0, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as integer", p.curToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}

	lit.Value = value

	return lit
}

// parse prefix (!, -) at cur token and advances tokens till end of current
// expression
func (p *Parser) parsePrefixExpression() ast.Expression {
	expression := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}

	// advance tokens to get right side of ! or -
	p.nextToken()

	expression.Right = p.parseExpression(PREFIX)

	return expression
}

// parse infix expressions. eg 5 * (10 + 5)
// takes in left of cur token (infix) and recursively calls parseExpression on
// the right side
func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	expression := &ast.InfixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Left:     left,
	}

	precedence := p.curPrecedence()
	p.nextToken()
	// make a recursive parseExpression call to get the expression for the right
	// side
	expression.Right = p.parseExpression(precedence)

	return expression
}
