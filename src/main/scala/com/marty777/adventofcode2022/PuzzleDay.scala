 package com.marty777.adventofcode2022
 
/*
	Generic interface for puzzle days. This might be too generic, but we'll see.
	If parsing the input file once for each part is too much overlapped work then
	it might need to be revised, but I recall there are puzzles where doing that 
	is necessary and it simplifies things for now.
*/
trait PuzzleDay[ParsedInput1, ParsedInput2, Result1, Result2] {
	
	def parse1(inputPath: String) : ParsedInput1
	def parse2(inputPath: String) : ParsedInput2
	
	def part1(parsedInput: ParsedInput1): Result1
	def part2(parsedInput: ParsedInput2): Result2
	
	def run(inputPath: String): (Result1, Result2) = {
		val result1 = part1(parse1(inputPath))
		val result2 = part2(parse2(inputPath))
		(result1, result2)
	}
}