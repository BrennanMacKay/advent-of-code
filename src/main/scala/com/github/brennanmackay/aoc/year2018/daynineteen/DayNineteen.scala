package com.github.brennanmackay.aoc.year2018.daynineteen

import com.github.brennanmackay.aoc.common.Common

object DayNineteen {
  def addr(inst: Instruction, reg: Array[Int]): Array[Int] = {
    val newReg = reg.clone()
    newReg(inst.c) = reg(inst.a) + reg(inst.b)
    newReg
  }

  def addi(inst: Instruction, reg: Array[Int]): Array[Int] = {
    val newReg = reg.clone()
    newReg(inst.c) = reg(inst.a) + inst.b
    newReg
  }

  def mulr(inst: Instruction, reg: Array[Int]): Array[Int] = {
    val newReg = reg.clone()
    newReg(inst.c) = reg(inst.a) * reg(inst.b)
    newReg
  }

  def muli(inst: Instruction, reg: Array[Int]): Array[Int] = {
    val newReg = reg.clone()
    newReg(inst.c) = reg(inst.a) * inst.b
    newReg
  }

  def banr(inst: Instruction, reg: Array[Int]): Array[Int] = {
    val newReg = reg.clone()
    newReg(inst.c) = reg(inst.a) & reg(inst.b)
    newReg
  }

  def bani(inst: Instruction, reg: Array[Int]): Array[Int] = {
    val newReg = reg.clone()
    newReg(inst.c) = reg(inst.a) & inst.b
    newReg
  }

  def borr(inst: Instruction, reg: Array[Int]): Array[Int] = {
    val newReg = reg.clone()
    newReg(inst.c) = reg(inst.a) | reg(inst.b)
    newReg
  }

  def bori(inst: Instruction, reg: Array[Int]): Array[Int] = {
    val newReg = reg.clone()
    newReg(inst.c) = reg(inst.a) | inst.b
    newReg
  }

  def setr(inst: Instruction, reg: Array[Int]): Array[Int] = {
    val newReg = reg.clone()
    newReg(inst.c) = reg(inst.a)
    newReg
  }

  def seti(inst: Instruction, reg: Array[Int]): Array[Int] = {
    val newReg = reg.clone()
    newReg(inst.c) = inst.a
    newReg
  }

  def gtir(inst: Instruction, reg: Array[Int]): Array[Int] = {
    val newReg = reg.clone()
    newReg(inst.c) = if (inst.a > reg(inst.b)) 1 else 0
    newReg
  }

  def gtri(inst: Instruction, reg: Array[Int]): Array[Int] = {
    val newReg = reg.clone()
    newReg(inst.c) = if (reg(inst.a) > inst.b) 1 else 0
    newReg
  }

  def gtrr(inst: Instruction, reg: Array[Int]): Array[Int] = {
    val newReg = reg.clone()
    newReg(inst.c) = if (reg(inst.a) > reg(inst.b)) 1 else 0
    newReg
  }

  def eqir(inst: Instruction, reg: Array[Int]): Array[Int] = {
    val newReg = reg.clone()
    newReg(inst.c) = if (inst.a == reg(inst.b)) 1 else 0
    newReg
  }

  def eqri(inst: Instruction, reg: Array[Int]): Array[Int] = {
    val newReg = reg.clone()
    newReg(inst.c) = if (reg(inst.a) == inst.b) 1 else 0
    newReg
  }

  def eqrr(inst: Instruction, reg: Array[Int]): Array[Int] = {
    val newReg = reg.clone()
    newReg(inst.c) = if (reg(inst.a) == reg(inst.b)) 1 else 0
    newReg
  }

  val ops =
    Map(
      "addr" -> Op(addr, "addr"),
      "addi" -> Op(addi, "addi"),
      "mulr" -> Op(mulr, "mulr"),
      "muli" -> Op(muli, "muli"),
      "banr" -> Op(banr, "banr"),
      "bani" -> Op(bani, "bani"),
      "borr" -> Op(borr, "borr"),
      "bori" -> Op(bori, "bori"),
      "setr" -> Op(setr, "setr"),
      "seti" -> Op(seti, "seti"),
      "gtir" -> Op(gtir, "gtir"),
      "gtri" -> Op(gtri, "gtri"),
      "gtrr" -> Op(gtrr, "gtrr"),
      "eqir" -> Op(eqir, "eqir"),
      "eqri" -> Op(eqri, "eqri"),
      "eqrr" -> Op(eqrr, "eqrr")
    )

  case class Instruction(op: String, a: Int, b: Int, c: Int)
  case class Op(op: (Instruction, Array[Int]) => Array[Int], name: String)

  case class State(regs: Array[Int],
                   instructions: Vector[Instruction],
                   ip: Int,
                   ipReg: Int) {
    def next: Option[State] = {
      if (ip >= instructions.size) {
        None
      } else {
        val preRunRegs = regs.clone()
        preRunRegs(ipReg) = ip
        val instruction = instructions(ip)
        val postRunRegs = ops(instruction.op).op(instruction, preRunRegs)
        val newIp = postRunRegs(ipReg)

        Some(this.copy(regs = postRunRegs, ip = newIp + 1))
      }
    }
    
    def optimizedNext: Option[State] = {
      if (ip == 3) {
        val nextRegs = regs.clone

        
        if (nextRegs(5) % nextRegs(3) == 0) {
          nextRegs(0) = nextRegs(0) + nextRegs(3)
        }
        
        nextRegs(1) = nextRegs(5) + 1
        this.copy(regs = nextRegs).next
      } else {
        next
      }
    }
    
    def view: Unit = {
      println(s"instruction pointer: $ip, regs: ${regs.mkString(", ")}")
    }
  }

  def loadProgram(name: String,
                  initialState: Array[Int] = Array.fill(6)(0)): State = {
    val instrRegex = """^(\w+) (\d+) (\d+) (\d+)$""".r
    val ipRegRegex = """^#ip (\d)$""".r

    val data = Common.loadData(name)
    val ipReg = data.head match {
      case ipRegRegex(ipReg) => ipReg.toInt
    }

    val instructions = data.tail.map {
      case instrRegex(op, i1, i2, i3) =>
        Instruction(op, i1.toInt, i2.toInt, i3.toInt)
      case x => throw new RuntimeException(s"Bad instr: $x")
    }.toVector

    State(initialState, instructions, 0, ipReg)
  }

  def run(state: State): State = {
    def rec(state: State, instCount: Array[Int], iter: Int = 0): State = {
      //instCount(state.ip) = instCount(state.ip) + 1

//      if (Seq(3).contains(state.ip)) {
//        state.view
//      }
      
      if (iter % 1000 == 0 && state.ip == 3) {
        //Thread.sleep(10)
        print(s"$iter ")
        state.view
        //println(instCount.zipWithIndex.map(_.swap).mkString(", "))
      }
      //state.view
      state.optimizedNext match {
        case Some(next) if iter == 1000000000 => state
        case Some(next) =>
          rec(next, instCount, if(state.ip == 3) iter + 1 else iter)
        case None       => state
      }
    }

    rec(state, Array.fill(state.instructions.size)(0))
  }

  def main(args: Array[String]): Unit = {
    val initialState =
      loadProgram("day-nineteen-input.txt", Array(1, 0, 0, 0, 0, 0))
    run(initialState).view
  }
}
