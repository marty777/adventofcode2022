package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day23Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day23Definitions {
	case class Coord(x:Int, y:Int)
}

object Day23 extends PuzzleDay[Seq[String], Seq[String], Long, Long] {
	override def parse1(inputPath: String): Seq[String] = readLines(inputPath)
	override def parse2(inputPath: String): Seq[String] = parse1(inputPath)

	override def part1(lines: Seq[String]): Long =  {
		var elves = parseLines(lines)
		for(step <- 0 until 10) {
			elves = elfStep(elves, step)
		}
		val width = elves.toSeq.map(e => e.x).max - elves.toSeq.map(e => e.x).min + 1
		val height = elves.toSeq.map(e => e.y).max - elves.toSeq.map(e => e.y).min + 1
		width * height - elves.size		
	}
	
	override def part2(lines: Seq[String]): Long =  {
		var elves = parseLines(lines)
		var steps = 0
		var done = false 
		while(!done) {
			val lastElves = elves
			elves = elfStep(elves, steps)
			if(elves == lastElves) {
				done = true
			}
			steps += 1
		}
		steps
	}
	
	def coordAdd(a:Coord, b:Coord) = Coord(a.x + b.x, a.y + b.y)
	def mapIncrement(c:Coord, theMap:collection.mutable.Map[Coord,Int]) = {
		if(!theMap.contains(c)) then theMap(c) = 1 else theMap(c) = theMap(c) + 1
	}
	
	def elfStep(elves:Set[Coord], step:Int):Set[Coord] = {
		val nextElves = collection.mutable.Set.empty[Coord]
		val elfProposals = collection.mutable.Map.empty[Coord, Coord]
		val elfProposalDestinations = collection.mutable.Map.empty[Coord,Int]
		val surroundings = Seq(Coord(-1,-1),Coord(0,-1),Coord(1,-1),Coord(1,0), Coord(1,1),Coord(0,1),Coord(-1,1),Coord(-1,0))
		val norths = surroundings.filter(c => c.y == -1)
		val souths = surroundings.filter(c => c.y == 1)
		val wests = surroundings.filter(c => c.x == -1)
		val easts = surroundings.filter(c => c.x == 1)
		for(elfPos <- elves) {
			val neightborCount = surroundings.foldLeft(0)((acc,s) => if(elves.contains(coordAdd(elfPos, s))) then acc + 1 else acc)
			if(neightborCount == 0) {
				elfProposals(elfPos) = elfPos
				mapIncrement(elfProposals(elfPos), elfProposalDestinations)
			}
			else {
				var proposalFound = false
				var directionIndex = 0
				while(directionIndex < 4 && !proposalFound) {
					val direction = (directionIndex + step) % 4
					direction match {
						case 0 => {
							if(!norths.foldLeft(false)((acc, p) => acc | elves.contains(coordAdd(p, elfPos)))) {
								proposalFound = true
								elfProposals(elfPos) =  coordAdd(elfPos, Coord(0, -1))
								mapIncrement(elfProposals(elfPos), elfProposalDestinations)
							}							
						}
						case 1 => {
							if(!souths.foldLeft(false)((acc, p) => acc | elves.contains(coordAdd(p, elfPos)))) {
								proposalFound = true
								elfProposals(elfPos) =  coordAdd(elfPos, Coord(0, 1))
								mapIncrement(elfProposals(elfPos), elfProposalDestinations)
							}						
						}
						case 2 => {
							if(!wests.foldLeft(false)((acc, p) => acc | elves.contains(coordAdd(p, elfPos)))) {
								proposalFound = true
								elfProposals(elfPos) =  coordAdd(elfPos, Coord(-1, 0))
								mapIncrement(elfProposals(elfPos), elfProposalDestinations)
							}							
						}
						case 3 => {
							if(!easts.foldLeft(false)((acc, p) => acc | elves.contains(coordAdd(p, elfPos)))) {
								proposalFound = true
								elfProposals(elfPos) = coordAdd(elfPos, Coord(1, 0))
								mapIncrement(elfProposals(elfPos), elfProposalDestinations)
							}							
						}
					}
					directionIndex += 1
				}
				if(!proposalFound) {
					elfProposals(elfPos) = elfPos
					mapIncrement(elfProposals(elfPos), elfProposalDestinations)
				}
			}
		}
		for(elfPos <- elves) {
			val proposed = elfProposals(elfPos)
			if(elfProposalDestinations(proposed) == 1) {
				nextElves += proposed
			}
			else {
				nextElves += elfPos
			}
		}
		nextElves.toSet
	}
	
	def parseLines(lines:Seq[String]): Set[Coord] = {
		val elves = collection.mutable.Set.empty[Coord]
		for(y <- 0 until lines.size) {
			val chars = lines(y).toCharArray
			for(x <- 0 until chars.size) {
				if(chars(x) == '#') {
					elves += Coord(x,y)
				}
			}
		}
		elves.toSet
	}
}

