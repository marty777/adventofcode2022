package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day03 extends PuzzleDay[Seq[String], Seq[String], Int, Int] {
	override def parse1(inputPath: String): Seq[String] = readLines(inputPath)
	override def parse2(inputPath: String): Seq[String] = readLines(inputPath)
	
	override def part1(rucksacks: Seq[String]): Int = rucksacks.map(rucksackPriority).sum
	override def part2(rucksacks: Seq[String]): Int = rucksacks.sliding(3,3).map(groupPriority).sum
	
	def itemPriority(char:Char):Int = {
		char match {
			case c if 'a' to 'z' contains c		=>	(c - 'a' + 1)
			case c if 'A' to 'Z' contains c		=> 	(c - 'A' + 27)
			case _								=> 	throw Exception(s"Score not found for unrecognized character '$char'")
		}
	}
	
	def rucksackPriority(rucksack:String): Int = {
		val intersection = rucksack.toCharArray.take(rucksack.length/2)
							.intersect(rucksack.toCharArray.takeRight(rucksack.length/2))
							.distinct
		intersection.size match {
			case 1	=>	itemPriority(intersection(0))
			case _	=>	throw Exception(s"Shared item not found for rucksack $rucksack")
		}		
	}
	
	def groupPriority(rucksackGroup: Seq[String]): Int = {
		if(rucksackGroup.size != 3) {
			throw Exception(s"Expected a rucksack group size of 3 (${rucksackGroup.size} provided)")
		}
		val intersection = rucksackGroup(0).toCharArray
							.intersect(rucksackGroup(1).toCharArray)
							.intersect(rucksackGroup(2).toCharArray)
							.distinct
		intersection.size match {
			case 1	=> itemPriority(intersection(0))
			case _	=> throw Exception(s"Could not find common item for group ($rucksackGroup)")
		}
	}
}