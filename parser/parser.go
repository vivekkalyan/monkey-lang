package parser

import (
	"monkey-lang/ast"
	"monkey-lang/lexer"
	"monkey-lang/token"
)

type Parser struct {
	l *lexer.Lexer

	curToken  token.Token
	peekToken token.Token
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{l: l}

	// read two tokens, so that curToken and peekToken are both set
	p.nextToken()
	p.nextToken()

	return p
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
		return false
	}
}

// parses the program to return an array of parsed statements (done by
// ParseStatment)
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
func (p *Parser) parseStatement() ast.Statement {
	switch p.curToken.Type {
	case token.LET:
		return p.parseLetStatment()
	default:
		return nil
	}
}

// parses lets statements. statements are expected to have the form
// <let> <identifier> <assign> <expr> <semicolon>
// returns a statement with Token, Name and Value
func (p *Parser) parseLetStatment() *ast.LetStatement {
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
