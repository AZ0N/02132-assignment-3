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
  val init  :: done :: borderTop :: borderBottom :: borderLeft :: borderRight :: forLoop :: blackCenter :: blackTop :: blackBottom :: blackLeft :: blackRight :: firstRow :: lastRow :: rowOneBlack :: rowOneWhite :: rowFiveInit :: rowFourInit :: rowFiveBlack :: rowFiveWhite :: rowFourWhite :: rowFourBlack:: Nil = Enum(22)
  val rowOne :: rowTwo :: rowThree :: rowFour :: rowFive :: Nil = Enum(5)
  val stateReg = RegInit(init)

  // Support registers
  val x = RegInit(0.U(32.W))
  val y = RegInit(0.U(32.W))
  val rowType = RegInit(0.U(32.W))
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
    is(forLoop) {
      io.address := 20.U * y + x
      when (io.dataRead === 0.U){
        stateReg := blackCenter
      } .otherwise {
        stateReg := done
      }
    }
    is(blackCenter){
      writeBlack(x, y)
      when (x > 1.U){ //if we can go left
        stateReg := blackLeft
      } .otherwise{
        when (x < 18.U){ //else if we can go right
          stateReg := blackRight
        } . otherwise{
          when (y > 1.U){ //else if we can go up
            stateReg := blackTop
          } .otherwise{
            when (y < 18.U){ //else if we can go down
              stateReg := blackBottom
            } .otherwise{
              loop()
            }
          }
        }
      }
    }
    is(blackLeft){
      writeBlack(x - 1.U, y)
      when (x < 18.U){ //if we can go right
        stateReg := blackRight
      } .otherwise{
        when (y > 1.U){ //else if we can go up
          stateReg := blackTop
        } .otherwise{
          when (y < 18.U){ //else if we can go down
            stateReg := blackBottom
          } .otherwise{
            loop()
          }
        }
      }
    }
    is(blackRight){
      writeBlack(x + 1.U, y)
        when (y > 1.U){ //else if we can go up
          stateReg := blackTop
        } .otherwise{
          when (y < 18.U){ //else if we can go down
            stateReg := blackBottom
          } .otherwise{
            loop()
          }
        }
    }
    is(blackTop) {
      writeBlack(x,y - 1.U)
      when ( y < 18.U){
        stateReg := blackBottom
      } .otherwise {
        loop()
      }
    }
    is(blackBottom) {
      writeBlack(x,y + 1.U)
      loop()
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
        x := 3.U
        y := 1.U
        rowType := rowFive
        stateReg := firstRow
      } .otherwise {
        y := y + 1.U
        stateReg := borderRight
      }
    }
    is(firstRow){
      io.address := 20.U * y + x + 400.U
      when(io.dataRead === 0.U){
        stateReg := rowOneBlack
      } .otherwise{
        stateReg := rowOneWhite
      }
    }
    is(lastRow){
      io.address := 20.U * y + x + 400.U
      when(io.dataRead === 0.U){
        stateReg := rowOneBlack
      } .otherwise{
        stateReg := rowOneWhite
      }
    }
    is(rowFiveInit){
      io.address := 20.U * y + x + 400.U
      when(io.dataRead === 0.U){
        stateReg := rowFiveBlack
      } .otherwise{
        stateReg := rowFiveWhite
      }
    }
    is(rowFourInit){
      io.address := 20.U * y + x + 400.U
      when(io.dataRead === 0.U){
        stateReg := rowFourBlack
      } .otherwise{
        stateReg := rowFourWhite
      }
    }
    is(rowOneBlack){
      writeBlack(x,y)
      when(x >= 16.U){
          when(rowType === rowFive){
          x := 1.U
          stateReg := rowFiveInit
          }.otherwise{
            x:=18.U
            stateReg := rowFourInit
          }
      }.otherwise{
        x := x + 5.U
        when(rowType === rowFive){
            stateReg := firstRow
          }.otherwise{
            stateReg := lastRow
          }
      }
      
    }
    is(rowOneWhite){
      stateReg := done
    }
    is(rowFiveBlack){
      writeBlack(x,y)
      x := 5.U
      stateReg := forLoop
    }
    is(rowFiveWhite){
      stateReg := done
    }
    is(rowFourBlack){
      writeBlack(x,y)
      x := 4.U
      stateReg := forLoop
    }
    is(rowFourWhite){
      stateReg := done
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

  def loop(): Unit = {
    
    when(x === 15.U && y === 18.U){
      stateReg := done
    } .otherwise {
      when (x >= 14.U){
        changeRow()
      } .otherwise {
        x := x + 5.U
        stateReg := forLoop
      }
    }
  }

  def changeRow(): Unit = {
    when (y === 18.U){
      stateReg := done
    } .otherwise {
      y := y + 1.U
      when(y === 17.U){
        x := 1.U
        stateReg := lastRow
        rowType := rowFour
      }.otherwise{
        when(rowType === rowOne){
          stateReg := forLoop
          rowType := rowThree
          x := 3.U
        } .otherwise {
          when(rowType === rowTwo){
            stateReg := rowFourInit
            rowType := rowFour
            x := 18.U
          } .otherwise {
            when(rowType === rowThree){
              stateReg := rowFiveInit
              rowType := rowFive
              x := 1.U
            } .otherwise {
              when(rowType === rowFour){
                stateReg := forLoop
                rowType := rowOne
                x := 1.U
              } .otherwise {
                when(rowType === rowFive){
                  stateReg := forLoop
                  rowType := rowTwo
                  x := 2.U
                }
              }
            }
          }
        }
      }
        
      
      
    }   
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
