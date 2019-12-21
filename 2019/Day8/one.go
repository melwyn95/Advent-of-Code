package main

import "fmt"

func toIntSlice(s string) []int {
	l := len(s)
	intSlice := make([]int, l)
	for i := 0; i < l; i++ {
		intSlice[i] = int(s[i] - '0')
	}
	return intSlice
}

func count(slice []int, x int) int {
	count := 0
	for i := range slice {
		if slice[i] == x {
			count++
		}
	}
	return count
}

func Solve1(image string, row, col int) []string {
	imagelen, layerlen, lidx := len(image), row*col, 0
	layers := make([]string, imagelen/layerlen)
	for i := 0; i < imagelen; i += layerlen {
		layers[lidx] = image[i : i+layerlen]
		lidx++
	}
	min, minrow := -1, -1
	for i := range layers {
		c := count(toIntSlice(layers[i]), 0)
		if min == -1 {
			min = c
			minrow = i
		} else if c < min {
			min = c
			minrow = i
		}
	}
	fmt.Println(count(toIntSlice(layers[minrow]), 1) * count(toIntSlice(layers[minrow]), 2))
	return layers
}
