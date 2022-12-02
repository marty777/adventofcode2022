package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day02Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day02Definitions {
	enum Shape:
		case Rock, Paper, Scissors
	enum Result:
		case Win, Loss, Tie
	case class GuideEntry(opponent:Shape, response:Shape, result:Result)
}
object Day02 extends PuzzleDay[Seq[GuideEntry], Seq[GuideEntry], Int, Int] {
	override def parse1(inputPath: String): Seq[GuideEntry] = readLines(inputPath).flatMap(parseGuideEntry1)
	override def parse2(inputPath: String): Seq[GuideEntry] = readLines(inputPath).flatMap(parseGuideEntry2)
	
	override def part1(entries: Seq[GuideEntry]): Int = entries.map(roundScore).sum
	override def part2(entries: Seq[GuideEntry]): Int = entries.map(roundScore).sum
		
	def abcToShape(abc:String):Shape = {
		abc match {
			case "A" => Shape.Rock
			case "B" => Shape.Paper
			case "C" => Shape.Scissors
			case _	=> throw Exception(s"Unknown shape found: $abc")
		}
	}
	
	def xyzToShape(xyz:String):Shape = {
		xyz match {
			case "X" => Shape.Rock
			case "Y" => Shape.Paper
			case "Z" => Shape.Scissors
			case _	=> throw Exception(s"Unknown shape found: $xyz")
		}
	}
	
	def xyzToResult(xyz:String):Result = {
		xyz match {
			case "X" => Result.Loss
			case "Y" => Result.Tie
			case "Z" => Result.Win
			case _	=> throw Exception(s"Unknown result found: $xyz")
		}
	}
	
	def resultFromShapes(opponent:Shape, response:Shape): Result = {
		opponent match {
			case Shape.Rock 	=> response match 	{	
														case Shape.Rock 	=> Result.Tie 
														case Shape.Paper	=> Result.Win 
														case Shape.Scissors	=> Result.Loss
													}
			case Shape.Paper 	=> response match 	{	
														case Shape.Rock 	=> Result.Loss 
														case Shape.Paper 	=> Result.Tie 
														case Shape.Scissors	=> Result.Win
													}
			case Shape.Scissors => response match 	{	
														case Shape.Rock 	=> Result.Win 
														case Shape.Paper 	=> Result.Loss 
														case Shape.Scissors	=> Result.Tie
													}		
		}
	}
	
	def shapeFromResult(opponent:Shape, result:Result): Shape = {
		opponent match {
			case Shape.Rock 	=> result match {	
													case Result.Loss	=> Shape.Scissors 
													case Result.Tie 	=> Shape.Rock
													case Result.Win 	=> Shape.Paper	
												}
			case Shape.Paper 	=> result match {	
													case Result.Loss	=> Shape.Rock 
													case Result.Tie 	=> Shape.Paper
													case Result.Win 	=> Shape.Scissors	
												}
			case Shape.Scissors => result match {	
													case Result.Loss	=> Shape.Paper 
													case Result.Tie 	=> Shape.Scissors
													case Result.Win 	=> Shape.Rock	
												}
		}
	}
	
	def parseGuideEntry1(input: String): Option[GuideEntry] = {
		val entry = "(.*) (.*)".r
		input match {
			case entry(opponent, response)	=> 	{
													val opponentShape = abcToShape(opponent)
													val responseShape = xyzToShape(response)
													Some(GuideEntry(opponentShape,responseShape, resultFromShapes(opponentShape, responseShape)))
												}
			case _							=> 	None
		}
	}
	
	def parseGuideEntry2(input: String): Option[GuideEntry] = {
		val entry = "(.*) (.*)".r
		input match {
			case entry(opponent, result) 	=> {
													val opponentShape = abcToShape(opponent)
													val roundResult = xyzToResult(result)
													Some(GuideEntry(opponentShape,shapeFromResult(opponentShape, roundResult), roundResult))
												}
			case _							=> 	None
		}
	}
	
	def resultScore(result:Result): Int = {
		result match {
			case Result.Win		=> 6
			case Result.Tie		=> 3
			case Result.Loss	=> 0
		}
	}
	
	def shapeScore(shape:Shape): Int = {
		shape match {
			case Shape.Rock			=> 1
			case Shape.Paper		=> 2
			case Shape.Scissors		=> 3
		}
	}
	
	def roundScore(entry:GuideEntry): Int = shapeScore(entry.response) + resultScore(entry.result)
}