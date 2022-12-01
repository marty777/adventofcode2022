package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day01 extends PuzzleDay[Seq[String], Seq[String], Int, Int] {
	override def parse1(inputPath: String): Seq[String] = readLines(inputPath)
	override def parse2(inputPath: String): Seq[String] = parse1(inputPath)
	
	override def part1(lines: Seq[String]): Int = {
		elfSums(lines).max
	}
	
	override def part2(lines: Seq[String]): Int = {
		var sortedElves = elfSums(lines).sorted.zipWithIndex
		sortedElves.filter((e, i) => i >= sortedElves.size - 3).map(_._1).sum
	}
	
	def elfSums(lines: Seq[String]): Seq[Int] = {
		var elfSums:Seq[Int] = Seq()
		elfSums = elfSums :+ 0
		var currSum = 0
		for (line <- lines) {
			if(line.length == 0) {
				elfSums = elfSums :+ 0
			}
			else {
				elfSums = elfSums.updated(elfSums.size - 1, elfSums(elfSums.size - 1) + line.toInt)
			}
		}
		elfSums
	}
}