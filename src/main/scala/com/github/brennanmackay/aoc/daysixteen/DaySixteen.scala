package com.github.brennanmackay.aoc.daysixteen

import com.github.brennanmackay.aoc.common.Common

object DaySixteen {

  case class Instruction(opcode: Int, a: Int, b: Int, c: Int)
  case class Op(op: (Instruction, Array[Int]) => Array[Int], name: String)
  case class Test(before: Array[Int], after: Array[Int], instruction: Instruction)

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

  
  
  
  def testOp(op: Op, test: Test): Boolean = {
    val result = op.op(test.instruction, test.before)
    val bool = result.deep == test.after.deep
    
    //println(s"$bool, OP: ${op.name} expected: ${test.after.mkString(", ")} actual: ${result.mkString(", ")}")
    
    bool
  }
  
  def assignOps(unknown: Seq[Op], tests: Seq[Test]): Map[Int, Op] = {
    def rec(unknown: Seq[Op], known: Map[Int, Op]): Map[Int, Op] = {
      //println(s"unknown: ${unknown.size}")
      if (unknown.isEmpty) {
        known
      } else {
        val res = tests.map(test => {
          val success = unknown.flatMap(op => {
            if(testOp(op, test)) {
              Some(op)
            } else {
              None
            }
          })
          (test, success, success.size)
        }).filter(_._3 == 1)
        
        val ops = res.flatMap(_._2).toSet
        println(s"ops located: ${ops.map(_.name).mkString(", ")}")
        
        val entries = res.map(k => {
          k._1.instruction.opcode -> k._2.head
        })
        
        rec(unknown diff ops.toSeq, known ++ entries)
      }
    }
    
    rec(unknown, Map())
  }

  def run(instruction: Instruction, reg: Array[Int], ops: Map[Int, Op]): Array[Int] = {
    val op = ops(instruction.opcode)
    val result = op.op(instruction, reg)
    //println(s"Op: ${op.name} ${instruction} input: ${reg.mkString(", ")} output: ${reg.mkString(", ")} ")
    result
  }

  
  def runProgram(instructions: Seq[Instruction], ops: Map[Int, Op]): Array[Int] = {
    def rec(rem: Seq[Instruction], reg: Array[Int]): Array[Int] = {
      if (rem.isEmpty) {
        reg
      } else {
        rec(rem.tail, run(rem.head, reg, ops))
      }
    }
    
    rec(instructions, Array(0,0,0,0))
  }

  def readTests(name: String): Seq[Test] = {
    val before = """^Before: \[(\d+), (\d+), (\d+), (\d+)]$""".r
    val after =  """^After:  \[(\d+), (\d+), (\d+), (\d+)]$""".r
    val instr = """^(\d+) (\d+) (\d+) (\d+)$""".r

    val data = Common.loadData(name).filter(!_.isEmpty)

    def rec(remain: Seq[String], tests: Seq[Test]): Seq[Test] = {
      val set = remain.take(3).toVector
      if (set.size != 3) {
        tests
      } else {
        val regBefore = set(0) match {
          case before(r0, r1, r2, r3) => Array(r0.toInt, r1.toInt, r2.toInt, r3.toInt)
          case x => throw new RuntimeException(s"Bad before : $x")
        }
        val regAfter = set(2) match {
          case after(r0, r1, r2, r3) => Array(r0.toInt, r1.toInt, r2.toInt, r3.toInt)
          case x => throw new RuntimeException(s"Bad after: $x")
        }
        val instruction = set(1) match {
          case instr(i0, i1, i2, i3) => Instruction(i0.toInt, i1.toInt, i2.toInt, i3.toInt)
          case x => throw new RuntimeException(s"Bad instr: $x")
        }

        rec(remain.drop(3), tests :+ Test(regBefore, regAfter, instruction))
      }
    }

    rec(data, Seq())
  }
  
  def readInstructions(name: String): Seq[Instruction] = {
    val instr = """^(\d+) (\d+) (\d+) (\d+)$""".r

    Common.loadData(name).map(_ match {
      case instr(i0, i1, i2, i3) => Instruction(i0.toInt, i1.toInt, i2.toInt, i3.toInt)
      case x => throw new RuntimeException(s"Bad instr: $x")
    })
  }



  def main(args: Array[String]): Unit = {
    val unknownOps =
      Seq(Op(addr, "addr"),
        Op(addi, "addr"),
        Op(mulr, "mulr"),
        Op(muli, "muli"),
        Op(banr, "banr"),
        Op(bani, "bani"),
        Op(borr, "borr"),
        Op(bori, "bori"),
        Op(setr, "setr"),
        Op(seti,"seti"),
        Op(gtir, "gtir"),
        Op(gtri, "gtri"),
        Op(gtrr, "gtrr"),
        Op(eqir, "eqir"),
        Op(eqri, "eqri"),
        Op(eqrr, "eqrr"))


    val tests = readTests("day-sixteen-p1-ken.txt")
    
    val res = tests.map(test => {
      unknownOps.map(op => {
        val res: Boolean = testOp(op, test)
        if (res) 1 else 0
      }).sum
    }).count(_ == 1)
    //println(res)
    
    val ops = assignOps(unknownOps, tests)
    
    println(ops.mkString("\n"))
    
    //println(ops)
    
    val program = readInstructions("day-sixteen-p2-ken.txt")
    val result = runProgram(program, ops)
    
    println(result.mkString(", "))
  }
}
