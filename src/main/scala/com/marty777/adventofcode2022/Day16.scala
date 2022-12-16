package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day16Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day16Definitions {
	case class Valve(id:String, flow:Int, tunnels:Seq[String])
	case class StateNode(pressure:Int, steps: Int, open:Seq[String], position:String)
}

object Day16 extends PuzzleDay[Seq[Valve], Seq[Valve], Long, Long] {
	override def parse1(inputPath: String): Seq[Valve] = readLines(inputPath).flatMap(parseValve)
	override def parse2(inputPath: String): Seq[Valve] = parse1(inputPath)

	override def part1(valves: Seq[Valve]): Long = {
		var valveMap:Map[String,Valve] = Map()
		for(valve <- valves) {
			valveMap = valveMap.updated(valve.id, valve)
		}
		// Some valves may not be accessible, and only some are important (with flow > 0).
		val accessible = dijkstra_Accessible(valveMap)
		val importantValves:Seq[String] = accessible.filter(v => valveMap(v).flow > 0)
		// build a cache of distances between all accessible valves
		var pathMap:Map[(String,String),Int] = Map()
		for(i <- 0 until accessible.size) {
			for(j <- 0 until accessible.size) {
				if(i != j) {
					pathMap = pathMap.updated( (accessible(i), accessible(j)), dijkstra_BestPath(valveMap, accessible(i), accessible(j)) )
				}
			}
		}
		// find the best possible pressure release by traversing the valves and opening the important ones.
		//dijkstra_ReleasePressure(valveMap, importantValves, pathMap, isPart1 = true)
		dijkstra_ReleasePressure(valveMap, importantValves, pathMap, isPart1 = true)
	}
	
	override def part2(valves: Seq[Valve]): Long = {
		var valveMap:Map[String,Valve] = Map()
		for(valve <- valves) {
			valveMap = valveMap.updated(valve.id, valve)
		}
		val accessible = dijkstra_Accessible(valveMap)
		val importantValves:Seq[String] = accessible.filter(v => valveMap(v).flow > 0)
		var pathMap:Map[(String,String),Int] = Map()
		for(i <- 0 until accessible.size) {
			for(j <- 0 until accessible.size) {
				if(i != j) {
					pathMap = pathMap.updated( (accessible(i), accessible(j)), dijkstra_BestPath(valveMap, accessible(i), accessible(j)) )
				}
			}
		}
		
		// Not very speedy, but this will finish in reasonable time.
		// The elephant and the player can be considered seperately.
		// Take the list of important (flow > 0) valves. Find all distinct ways
		// to partition them between the elephant and the player.
		// For each partitioning, find the best pressure result for an actor
		// opening each set of important valves. Continue until all possible 
		// partitionings have been examined. Return the best result.
		
		if(importantValves.size > 6) {
			println("This may take a few minutes...")
		}
		// Caching the best pressures for previously seen partitionings shouldn't 
		// do better than halving the number of searches, but it's an improvement.
		val cachedPressures:scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map()
		val bitsMax = 1 << importantValves.size
		var bestPressure = -1
		for(bits <- 0 until bitsMax) {
			var myValves:Seq[String] = importantValves.zipWithIndex.filter((v,i) => ((bits >> i) & 0x01) == 0).map(_._1)
			var elephantValves:Seq[String] = importantValves.zipWithIndex.filter((v,i) => ((bits >> i) & 0x01) == 1).map(_._1)
			val myKey = pressureKey(myValves)
			val elephantKey = pressureKey(elephantValves)
			var myPressure = 0
			if(cachedPressures.contains(myKey)) {
				myPressure = cachedPressures(myKey)
			}
			else {
				myPressure = dijkstra_ReleasePressure(valveMap, myValves, pathMap, isPart1 = false)
				cachedPressures(myKey) = myPressure
			}
			var elephantPressure = 0
			if(cachedPressures.contains(elephantKey)) {
				elephantPressure = cachedPressures(elephantKey)
			}
			else {
				elephantPressure = dijkstra_ReleasePressure(valveMap, elephantValves, pathMap, isPart1 = false)
				cachedPressures(elephantKey) = elephantPressure
			}
			if(myPressure + elephantPressure > bestPressure) {
				bestPressure = myPressure + elephantPressure 
			}
		}
		bestPressure
	}
	
	def parseValve(input:String): Option[Valve] = {
		val valvePlural = "Valve (.*) has flow rate=(.*); tunnels lead to valves (.*)".r
		val valveSingular = "Valve (.*) has flow rate=(.*); tunnel leads to valve (.*)".r
		input match {
			case valvePlural(id, flowRate, tunnels) => {
				Some(Valve(id, flowRate.toInt, tunnels.split(", ").toSeq))
			}
			case valveSingular(id, flowRate, tunnel) => {
				Some(Valve(id, flowRate.toInt, Seq(tunnel)))
			}
			case _ => None
		}
	}
	
	// find all accessible valves in the system 
	def dijkstra_Accessible(valves:Map[String, Valve]): Seq[String] = {
		val frontier:scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map()
		val frontierNext:scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map()
		val explored:scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map()
		
		frontierNext("AA") = 0
		
		while(frontierNext.size > 0) {
			frontier.clear
			for(id <- frontierNext.keys) {
				frontier(id) = frontierNext(id)
			}
			frontierNext.clear
			for(id <- frontier.keys) {
				if(betterDist(explored, id, frontier(id))) {
					explored(id) = frontier(id)
				}
				val nextDist = frontier(id) + 1
				for(nextId <- valves(id).tunnels) {
					if(betterDist(explored,nextId,nextDist) && betterDist(frontierNext, nextId, nextDist) ) {
						frontierNext(nextId) = nextDist
					}
				}
			}
		}
		explored.toSeq.map(_._1)
	}
	
	// find the best distance between two accessible valves
	def dijkstra_BestPath(valves:Map[String, Valve], start:String, end:String): Int = {
		val frontier:scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map()
		val frontierNext:scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map()
		val explored:scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map()
		
		frontierNext(start) = 0
		
		while(frontierNext.size > 0) {
			frontier.clear
			for(id <- frontierNext.keys) {
				frontier(id) = frontierNext(id)
			}
			frontierNext.clear
			for(id <- frontier.keys) {
				if(betterDist(explored, id, frontier(id))) {
					explored(id) = frontier(id)
				}
				val nextDist = frontier(id) + 1
				for(nextId <- valves(id).tunnels) {
					if(betterDist(explored,nextId,nextDist) && betterDist(frontierNext, nextId, nextDist) ) {
						frontierNext(nextId) = nextDist
					}
				}
			}
		}
		if(!explored.contains(end)) {
			throw Exception(s"Could not find path between $start and $end")
		}
		explored(end)
	}
	
	// find the maximum amount of pressure released by a single actor opening all the important valves
	def dijkstra_ReleasePressure(valves:Map[String, Valve], importantValves:Seq[String], paths:Map[(String, String), Int], isPart1:Boolean): Int = {
		val frontier:scala.collection.mutable.Map[String, StateNode] = scala.collection.mutable.Map()
		val frontierNext:scala.collection.mutable.Map[String, StateNode] = scala.collection.mutable.Map()
		val explored:scala.collection.mutable.Map[String, StateNode] = scala.collection.mutable.Map()
		
		var stepsCutoff = 30
		if(!isPart1) {
			stepsCutoff = 26
		}
		
		frontierNext(stateKey(Seq(), "AA")) = StateNode(0,0,Seq(), "AA")
		
		while(frontierNext.size > 0) {
			frontier.clear
			for(key <- frontierNext.keys) {
				frontier(key) = frontierNext(key)
			}
			frontierNext.clear
			for(key <- frontier.keys) {
				if(betterPressure(explored, key, frontier(key).pressure)) {
					explored(key) = frontier(key)
				}
				for(important <- importantValves) {
					if(!frontier(key).open.contains(important)) {
						val nextSteps = frontier(key).steps + paths((frontier(key).position, important)) + 1
						val nextPressure = frontier(key).pressure + (valves(important).flow * (stepsCutoff - nextSteps))
						val nextOpen =  frontier(key).open :+ important
						val nextKey = stateKey(nextOpen, frontier(key).position)
						if(nextSteps <= stepsCutoff && betterPressure(explored, nextKey, nextPressure) && betterPressure(frontierNext, nextKey, nextPressure)) {
							frontierNext(nextKey) = StateNode(nextPressure, nextSteps, nextOpen, important)
						}
					}
				}
			}
		}
		explored.toSeq.map(_._2.pressure).sorted.reverse.head
	}
	
	// dijkstra helper functions
	def stateKey(openValves: Seq[String], position:String): String = {
		position + ":" + openValves.sorted.mkString("")
	}
	def pressureKey(visited: Seq[String]): String = {
		visited.sorted.mkString("")
	}
	def betterDist(collection: scala.collection.mutable.Map[String, Int], id:String, newDist:Int): Boolean = {
		if(!collection.contains(id)) then true else collection(id) > newDist
	}
	def betterPressure(collection: scala.collection.mutable.Map[String, StateNode], key:String, newPressure:Int): Boolean = {
		if(!collection.contains(key)) then true else collection(key).pressure < newPressure
	}
	
	
}

