package main

import (
	"io/ioutil"
)

func main() {
	tc1, err := ioutil.ReadFile("./t1")
	if err == nil {
		Solve1(string(tc1))
	}

	tc2, err := ioutil.ReadFile("./t2")
	if err == nil {
		Solve1(string(tc2))
		// Solve2(string(tc2))
	}

	// tc3, err := ioutil.ReadFile("./t3")
	// if err == nil {
	// 	Solve2(string(tc3))
	// }

}
