package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day12Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day12Definitions {
	case class Coord(x:Int, y:Int)
	case class State(grid:Seq[Seq[Char]], width:Int, height:Int, startCoord:Coord, endCoord:Coord)
}

object Day12 extends PuzzleDay[State, State, Int, Int] {
	override def parse1(inputPath: String): State = parseLines(readLines(inputPath))
	override def parse2(inputPath: String): State = parse1(inputPath)
	
	override def part1(state: State): Int = {
		dijkstra(state, isPart2 = false)
	}
	
	override def part2(state: State): Int = {
		dijkstra(state, isPart2 = true)
	}
	
	def dijkstra(state:State, isPart2:Boolean = false): Int = {
		val frontier:scala.collection.mutable.Map[Coord, Int] = scala.collection.mutable.Map()
		val frontierNext:scala.collection.mutable.Map[Coord, Int] = scala.collection.mutable.Map()
		val explored:scala.collection.mutable.Map[Coord, Int] = scala.collection.mutable.Map()
		val moves = List((0,1), (1,0), (0,-1), (-1,0))
		if(isPart2) {
			// add the end node at distance 0
			frontierNext(state.endCoord) = 0
		}
		else {
			// add the start node at distance 0
			frontierNext(state.startCoord) = 0
		}
		while(frontierNext.size > 0) {
			frontier.clear
			for(coord <- frontierNext.keys) {
				frontier(coord) = frontierNext(coord)
			}
			frontierNext.clear
			for(coord <- frontier.keys) {
				if(betterDist(explored, coord, frontier(coord))) {
					explored(coord) = frontier(coord)
				}
				val nextDist = frontier(coord) + 1
				for(move <- moves) {
					val nextCoord = Coord(coord.x + move._1, coord.y + move._2)
					if(inBounds(state, nextCoord)) {
						val delta = height(state, nextCoord) - height(state, coord)
						if(allowedDelta(delta, isPart2) 
							&& betterDist(explored,nextCoord,nextDist) 
							&& betterDist(frontierNext, nextCoord, nextDist)) {
							frontierNext(nextCoord) = nextDist
						}
					}
				}
			}
		}
		
		if(isPart2) {
			val as = explored.toSeq.filter((coord,node) => height(state, coord) == 'a').map(_._2)
			if(as.size < 1) {
				throw Exception("Couldn't find any paths")
			}
			else {
				as.sorted.head
			}
		}
		else {
			if(!explored.contains(state.endCoord)) {
				throw Exception("Couldn't reach the end node")
			}
			else {
				explored(state.endCoord)
			}
		}
	}
	
	// dijkstra helper functions
	def height(state: State, coord:Coord): Char = state.grid(coord.y)(coord.x)
	def inBounds(state:State, coord:Coord): Boolean = !(coord.x < 0 || coord.x >= state.width || coord.y < 0 || coord.y >= state.height)
	def betterDist(collection: scala.collection.mutable.Map[Coord, Int], coord:Coord, newDist:Int): Boolean = {
		if(!collection.contains(coord)) {
			true
		}
		else {
			collection(coord) > newDist
		}
	}
	def allowedDelta(delta:Int, isPart2:Boolean):Boolean = (isPart2 && delta >= -1) || (!isPart2 && delta <= 1)

	def parseLines(lines:Seq[String]): State = {
		var grid = lines.map(_.replace("S", "a")).map(_.replace("E", "z")).foldLeft(Seq.newBuilder[Seq[Char]])(_ += _.toCharArray.toSeq).result()
		if(grid.size == 0 || grid(0).size == 0) {
			throw Exception("Grid parsed with one or both dimensions 0")
		}
		var start = Coord(0,0)
		var end = Coord(0,0)
		for(i <- 0 until lines.size) {
			val s = lines(i).indexOf("S")
			val e = lines(i).indexOf("E")
			if(s != -1) {
				start = Coord(s,i)
			}
			if(e != -1) {
				end = Coord(e,i)
			}
		}
		if(start == Coord(0,0) && end == Coord(0,0)) {
			throw Exception("Unable to determine start and end points when parsing grid")
		}
		State(grid, grid(0).size, grid.size, start, end)
	}
}
