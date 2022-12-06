package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day06 extends PuzzleDay[Seq[String], Seq[String], Int, Int] {
	override def parse1(inputPath: String): Seq[String] = readLines(inputPath)
	override def parse2(inputPath: String): Seq[String] = parse1(inputPath)
	
	override def part1(lines: Seq[String]): Int = firstDistinctIndex(lines(0).toSeq, 4)	
	override def part2(lines: Seq[String]): Int = firstDistinctIndex(lines(0).toSeq, 14)
	
	def firstDistinctIndex(input:Seq[Char], toTake:Int): Int = {
		input.zipWithIndex.sliding(toTake).filter(x => x.map(_._1).distinct.size == toTake).toList(0)(toTake - 1)._2 + 1
	}
}