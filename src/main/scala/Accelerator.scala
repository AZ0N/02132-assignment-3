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
  val init :: loop :: up :: down :: left :: right :: black :: blackToBlack :: blackBelow :: white :: done :: borderOne :: borderLine :: Nil = Enum(13)
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
      x := 1.U
      y := 0.U
      borderAdress := 401.U
      //saves a clock cycle by writing in init
      writeBorder(400.U)
      
      when (io.start) {
        stateReg := borderOne
      } .otherwise {
        stateReg := init
      }
    }
    is(loop) {
      when (y >= 18.U){
        when ( x === 18.U){
          stateReg := done
        } .otherwise{
                x := x + 1.U
                y := 1.U
            when (nextLine(1.U) === 1.U){
              nextLine(1.U) := 0.U
              writeBlack(x + 1.U, 1.U)
            } .otherwise {
                io.writeEnable := false.B
                checkPixel(20.U + (x + 1.U), blackToBlack, left)
              }
          }  
      } .otherwise{
        when (nextLine(y+1.U) === 1.U){
          nextLine(y+1.U) := 0.U
          writeBlack(x, y + 1.U)
          y := y + 1.U
        } .otherwise {
            io.writeEnable := false.B
            y := y + 1.U
            checkPixel(20.U * (y + 1.U) + x, blackToBlack, left)}
        }
    }
    is(up) {
      checkPixel(20.U * y + x + 1.U, black, down)
    }
    is(down) {
      checkPixel(20.U * y + x - 1.U, black, white)
    }
    is(left) {
      checkPixel(20.U * (y + 1.U) + x, black, right)
      when (io.dataRead === 255.U){
        nextIsWhite := true.B
      }
    }
    is(right) {
      checkPixel(20.U * (y - 1.U) + x, black, up)
    }
    is(black) {
      writeBlack(x, y)
      nextIsWhiteCheck(nextIsWhite)
    }
    is(blackToBlack){
      writeBlack(x, y)
      nextLine(y) := 1.U
        when (y === 18.U){
          stateReg := loop
        } . otherwise {
          when (nextLine(y+1.U) === 1.U){
            stateReg := loop
          } otherwise {
            y := y + 1.U
            stateReg := black
          }
        }
    }
    is(white) {
      writeWhite(x, y)
      nextIsWhiteCheck(nextIsWhite)
    }
    is(done) {
      io.done := true.B
      stateReg := done
    }

    is(borderOne){
      writeBorder(borderAdress)
      borderAdress := borderAdress + 1.U
      when(borderAdress <= 418.U || borderAdress >= 779.U){
        when(borderAdress >= 799.U){
          stateReg := loop
        } .otherwise {
        stateReg := borderOne
        }
      } .otherwise {
          stateReg := borderLine
        }
    }
    is(borderLine){
      writeBorder(borderAdress)
      borderAdress := borderAdress + 19.U
      stateReg := borderOne
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

  def writeBorder(borderAdress: UInt): Unit = {
    io.address := borderAdress
    io.dataWrite := 255.U
    io.writeEnable := true.B
  }

  def checkPixel(address: UInt, blackState: UInt, whiteState: UInt): Unit = {
    io.address := address
    when (io.dataRead === 0.U) {
      when(nextLine(y +1.U) === 1.U){
        stateReg := black
      } .otherwise {
      stateReg := blackState
      }
    } .otherwise {
      stateReg := whiteState
    }
  }
  
  def nextIsWhiteCheck(nextIsWhite : Bool) = {
      nextIsWhite := false.B
    when (nextLine(y+1.U) === 1.U){
      stateReg := loop
    } .otherwise{
    when (nextIsWhite === true.B){
        when ( y === 18.U){
          stateReg := loop
        } .otherwise{
          y := y + 1.U
          stateReg := left
        }
      } .otherwise {
        stateReg := loop
      }
    }
  }
}
