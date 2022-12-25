package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day24Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day24Definitions {
	case class Coord(x:Int, y:Int, c:Int)
	case class Valley(width:Int, height:Int, cycleLength:Int, blizzardPositions: Map[Coord, Int])
}

object Day24 extends PuzzleDay[Seq[String], Seq[String], Int, Int] {
	override def parse1(inputPath: String): Seq[String] = readLines(inputPath)
	override def parse2(inputPath: String): Seq[String] = parse1(inputPath)

	override def part1(lines: Seq[String]): Int =  {
		val valley = parseLines(lines)
		dijkstra(valley, Map(Coord(0,-1,0) -> 0), toExit = true).values.min
	}
	
	override def part2(lines: Seq[String]): Int =  {
		val valley = parseLines(lines)
		val reachedExitOnce = dijkstra(valley, Map(Coord(0,-1,0) -> 0), toExit = true)
		val reachedEntranceAgain = dijkstra(valley, reachedExitOnce, toExit = false)
		dijkstra(valley, reachedEntranceAgain, toExit = true).values.min
	}
	
	def gcd(a:Int, b:Int):Int = if(b==0) then a.abs else gcd(b, a % b)
	def lcm(a:Int, b:Int):Int = (a*b).abs/gcd(a,b)
	def mapIncrement(theMap:collection.mutable.Map[Coord,Int], coord:Coord) = if(theMap.contains(coord)) then theMap(coord) = theMap(coord) + 1 else theMap(coord) = 1
	def nonNegativeMod(a:Int, b:Int):Int = if (a % b < 0) then (a % b) + b else (a % b) 
	def betterSteps(theMap:collection.mutable.Map[Coord,Int], coord:Coord, newSteps:Int):Boolean = if(!theMap.contains(coord)) then true else theMap(coord) > newSteps
	def okayToAdd(blizzardMap:Map[Coord,Int], explored:collection.mutable.Map[Coord,Int], frontierNext:collection.mutable.Map[Coord,Int], coord:Coord, newSteps:Int):Boolean = {
		!blizzardMap.contains(coord) && betterSteps(explored, coord, newSteps) && betterSteps(frontierNext, coord, newSteps)
	}
	
	def dijkstra(valley:Valley, startStates:Map[Coord,Int], toExit:Boolean):Map[Coord,Int] = {
		val explored = collection.mutable.Map.empty[Coord, Int]
		val frontier = collection.mutable.Map.empty[Coord, Int]
		val frontierNext = collection.mutable.Map.empty[Coord,Int]
		val reachedDestination = collection.mutable.Map.empty[Coord,Int]
		
		for(k <- startStates.keys) {
			frontierNext(k) = startStates(k)
		}
	
		while(frontierNext.size > 0) {
			frontier.clear()
			for(k <- frontierNext.keys) {
				frontier(k) = frontierNext(k)
			}
			frontierNext.clear
			for(k <- frontier.keys) {
				if(betterSteps(explored, k, frontier(k))) {
					explored(k) = frontier(k)
				}
				val nextSteps = frontier(k) + 1
				// if at the entrance and going to the exit
				if(k.x == 0 && k.y == -1 && toExit) {
					// options are wait and down
					val wait = Coord(k.x,k.y,(k.c + 1) % valley.cycleLength)
					if(okayToAdd(valley.blizzardPositions, explored, frontierNext, wait, nextSteps)) {
						frontierNext(wait) = nextSteps
					}
					val down = Coord(k.x,k.y + 1,(k.c + 1) % valley.cycleLength)
					if(okayToAdd(valley.blizzardPositions, explored, frontierNext, down, nextSteps)) {
						frontierNext(down) = nextSteps
					}
				}
				// if adjacent to the entrance and going to the entrance
				else if(k.x == 0 && k.y == 0 && !toExit) {
					val startCoord = Coord(k.x, k.y - 1, (k.c + 1) % valley.cycleLength)
					if(betterSteps(reachedDestination, startCoord, nextSteps)) {
						reachedDestination(startCoord) = nextSteps
					}
				}
				// if at the exit and going to the entrance
				else if(k.x == valley.width - 1 && k.y == valley.height && !toExit) {
					// options are wait and up
					val wait = Coord(k.x,k.y,(k.c + 1) % valley.cycleLength)
					if(okayToAdd(valley.blizzardPositions, explored, frontierNext, wait, nextSteps)) {
						frontierNext(wait) = nextSteps
					}
					val up = Coord(k.x,k.y - 1,(k.c + 1) % valley.cycleLength)
					if(okayToAdd(valley.blizzardPositions, explored, frontierNext, up, nextSteps)) {
						frontierNext(up) = nextSteps
					}
				}
				// if adjacent to the exit and going to the exit
				else if(k.x == valley.width - 1 && k.y == valley.height - 1 && toExit) {
					val endCoord = Coord(k.x, k.y + 1, (k.c + 1) % valley.cycleLength)
					if(betterSteps(reachedDestination, endCoord, nextSteps)) {
						reachedDestination(endCoord) = nextSteps
					}
				}
				// everything else
				else {
					for(i <- 0 until 5) {
						var nextCoord = Coord(0,0,0)
						i match {
							case 0 =>  nextCoord = Coord(k.x,k.y,(k.c + 1) % valley.cycleLength) // wait
							case 1 =>  nextCoord = Coord(k.x,k.y + 1,(k.c + 1) % valley.cycleLength) // down
							case 2 => nextCoord = Coord(k.x,k.y - 1,(k.c + 1) % valley.cycleLength) // up
							case 3 => nextCoord = Coord(k.x + 1,k.y,(k.c + 1) % valley.cycleLength) // right
							case 4 => nextCoord = Coord(k.x - 1,k.y,(k.c + 1) % valley.cycleLength) // left
						}
						if(nextCoord.x >= 0 && nextCoord.x < valley.width && nextCoord.y >= 0 && nextCoord.y < valley.height) {
							if(okayToAdd(valley.blizzardPositions, explored, frontierNext, nextCoord, nextSteps)) {
								frontierNext(nextCoord) = nextSteps
							}
						}
					}
				}
			}
		}
		reachedDestination.toMap
	}
	
	def parseLines(lines:Seq[String]):Valley = {
		var blizzards = Seq.empty[(Int, Int, Int)]
		val width = lines(0).length - 2
		val height = lines.size - 2
		for(y <- 1 until lines.size - 1) {
			val chars = lines(y).toCharArray
			for(x <- 1 until chars.size - 1) {
				chars(x) match {
					case '^' => blizzards = blizzards :+ (x-1,y-1,0)
					case 'v' => blizzards = blizzards :+ (x-1,y-1,1)
					case '<' => blizzards = blizzards :+ (x-1,y-1,2)
					case '>' => blizzards = blizzards :+ (x-1,y-1,3)
					case _ => {}
				}
			}
		}
		// build a lookup of all blizzard-occupied positions at each step in the cycle
		val cycleLength = lcm(height, width)
		val blizzardPositions = collection.mutable.Map.empty[Coord, Int]
		for(i <- 0 until cycleLength) {
			for(j <- 0 until blizzards.size) {
				var pos:Coord = Coord(0,0,i)
				blizzards(j)._3 match {
					case 0 => {
						pos = Coord(blizzards(j)._1, nonNegativeMod(blizzards(j)._2 - i, height), i)
					}
					case 1 => {
						pos = Coord(blizzards(j)._1, nonNegativeMod(blizzards(j)._2 + i, height), i)
					}
					case 2 => {
						pos = Coord(nonNegativeMod(blizzards(j)._1 - i, width), blizzards(j)._2, i)
					}
					case 3 => {
						pos = Coord(nonNegativeMod(blizzards(j)._1 + i, width), blizzards(j)._2, i)
					}
				}
				mapIncrement(blizzardPositions, pos)
			}
		}
		Valley(width, height, cycleLength, blizzardPositions.toMap)
	}
}

