package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day01 extends PuzzleDay[Seq[String], Seq[String], Int, Int] {
	override def parse1(inputPath: String): Seq[String] = readLines(inputPath)
	override def parse2(inputPath: String): Seq[String] = parse1(inputPath)
	
	override def part1(input: Seq[String]): Int = {
		input.map(println)
		1
	}
	
	override def part2(input: Seq[String]): Int = {
		2
	}
	
}