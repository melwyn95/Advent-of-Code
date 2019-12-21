package main

import "fmt"

func getIntLayer(layer string, row, col int) [][]int {
	img := make([][]int, row)
	for i := range layer {
		img[i/col] = append(img[i/col], int(layer[i]-'0'))
	}
	return img
}

func getDummyFinalImage(row, col int) [][]int {
	final := make([][]int, row)
	for i := range final {
		final[i] = make([]int, col)
		for j := range final[i] {
			final[i][j] = -1
		}
	}
	return final
}

func Solve2(image string, row, col int) {
	layersstring := Solve1(image, row, col)
	final := getDummyFinalImage(row, col)
	layers := make([][][]int, len(layersstring))
	for i := range layersstring {
		layers[i] = getIntLayer(layersstring[i], row, col)
		for r := 0; r < row; r++ {
			for c := 0; c < col; c++ {
				if final[r][c] == -1 && layers[i][r][c] != 2 {
					final[r][c] = layers[i][r][c]
				}
			}
		}
	}
	for r := range final {
		for c := range final[r] {
			if final[r][c] == 1 {
				fmt.Print("*")
			} else {
				fmt.Print(" ")
			}
		}
		fmt.Println()
	}
}
