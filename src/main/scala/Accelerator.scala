import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val dataRead = Input(UInt (32.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))

  })

  // State enum and register
  val init :: done :: borderTop :: borderBottom :: borderLeft :: borderRight :: Nil = Enum(6)
  val stateReg = RegInit(init)

  // Support registers
  val x = RegInit(0.U(32.W))
  val y = RegInit(0.U(32.W))
  val borderAdress = RegInit(0.U(32.W))
  val nextIsWhite = RegInit(false.B)
  val nextLine = Reg(Vec(18, UInt(2.W)))

  // Default values
  io.done := false.B
  io.address := 0.U(16.W)
  io.writeEnable := false.B
  io.dataWrite := 0.U(32.W)

  // FSMD Switch
  switch(stateReg) {
    is(init) {
      x := 0.U
      y := 0.U
      writeBlack(0.U,0.U)
      
      when (io.start) {
        stateReg := borderTop
      } .otherwise {
        stateReg := init
      }
    }
    is(done) {
      io.done := true.B
      stateReg := done
    }
    is(borderTop) {
      writeBlack(x + 1.U,y)
      when (x === 18.U) {
        stateReg := borderBottom
        x := 0.U
        y := 19.U
      } .otherwise {
        x := x + 1.U
        stateReg := borderTop
      }
    }
    is(borderBottom) {      
      writeBlack(x + 1.U,y)
      when (x === 18.U) {
        x := 0.U
        y := 0.U
        stateReg := borderLeft
      } .otherwise {
        x := x + 1.U
        stateReg := borderBottom
      }
    }
    is(borderLeft) {
      writeBlack(x,y + 1.U)
      when (y === 18.U) {
        x := 19.U
        y := 0.U
        stateReg := borderRight
      } .otherwise {
        y := y + 1.U
        stateReg := borderLeft
      }
    }
    is(borderRight) {
      writeBlack(x,y + 1.U)
      when (y === 17.U) {
        x := 1.U
        y := 1.U
        stateReg := done
      } .otherwise {
        y := y + 1.U
        stateReg := borderRight
      }
    }
  }

  def writeBlack(x: UInt, y: UInt): Unit = {
    io.address := 20.U * y + x + 400.U
    io.dataWrite := 255.U
    io.writeEnable := true.B
  }

  def writeWhite(x: UInt, y: UInt): Unit = {
    io.address := 20.U * y + x + 400.U
    io.dataWrite := 0.U
    io.writeEnable := true.B
  }

  def checkPixel(x : UInt, y : UInt, blackState: UInt, whiteState: UInt): Unit = {
    io.address := 20.U * y + x + 400.U
    return io.dataRead === 0.U
  }
  // def checkPixel(address: UInt, blackState: UInt, whiteState: UInt): Unit = {
  //   io.address := address
  //   when (io.dataRead === 0.U) {
  //     when(nextLine(y +1.U) === 1.U){
  //       stateReg := black
  //     } .otherwise {
  //     stateReg := blackState
  //     }
  //   } .otherwise {
  //     stateReg := whiteState
  //   }
  // }
}
