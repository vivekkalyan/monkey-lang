package main

import (
	"fmt"
	"monkey-lang/repl"
	"os"
	"os/user"
)

func main() {
	user, err := user.Current()
	if err != nil {
		panic(err)
	}
	fmt.Printf("Hello %s! This is the Monkey programming language!\n", user.Username)
	fmt.Printf("Feel free to type some commands\n")
	repl.Start(os.Stdin, os.Stdout)
}
