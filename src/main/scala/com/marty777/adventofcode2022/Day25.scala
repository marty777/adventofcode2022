package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day25 extends PuzzleDay[Seq[String], Seq[String], String, String] {
	override def parse1(inputPath: String): Seq[String] = readLines(inputPath)
	override def parse2(inputPath: String): Seq[String] = parse1(inputPath)

	override def part1(lines: Seq[String]): String =  {
		val snafus = lines.map(parseSNAFU)
		snafus.sum
		toSNAFU(snafus.sum)
	}
	
	override def part2(lines: Seq[String]): String =  {
		"To the North Pole!"
	}
	
	// Doesn't handle negative SNAFU numbers.
	// The math for this is probably interesting, but just brute force
	// the digits.
	def toSNAFU(input:Long):String = {
		if(input < 0) {
			throw Exception(s"Unable to generate SNAFU negative value $input")
		}
		val digits = Seq('=', '-', '0', '1', '2')
		var maxPosition = 0
		var foundPosition = false
		while(!foundPosition) {
			if(math.pow(5,maxPosition) * 2 > input) {
				foundPosition = true 
			}
			maxPosition += 1
		}
		var chars = collection.mutable.Seq.empty[Char]
		for(i <- 0 until maxPosition) {
			chars = chars :+ '0'
		}
		for(i <- 0 until maxPosition) {
			var bestDiff:Long = 0
			var bestChar = '='
			for(j <- 0 until digits.size) {
				chars(i) = digits(j)
				val testDiff = (parseSNAFU(chars.mkString("")) - input).abs
				if(j == 0 || bestDiff > testDiff) {
					bestDiff = testDiff
					bestChar = digits(j)
				}
			}
			chars(i) = bestChar
		}
		val snafu = chars.mkString("")
		if(parseSNAFU(snafu) != input) {
			throw Exception(s"Unable to generate SNAFU for input $input")
		}
		snafu
	}
	
	def parseSNAFU(line:String):Long = {
		val chars = line.toCharArray
		var sum:Long = 0
		for(i <- 0 until chars.size) {
			val position = chars.size - i - 1
			val exponent = scala.math.pow(5, position).toLong
			var value:Long = 0
			chars(i) match {
				case '0' => value = exponent * 0
				case '1' => value = exponent * 1
				case '2' => value = exponent * 2
				case '-' => value = exponent * -1
				case '=' => value = exponent * -2
			}
			sum += value
		}
		sum
	}
}

