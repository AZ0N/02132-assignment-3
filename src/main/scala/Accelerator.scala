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
  val init  :: done :: border :: specialRow :: borderTop :: borderBottom :: borderLeft :: borderRight :: forLoop :: blackCross :: firstRow :: lastRow :: rowBlack :: rowWhite :: rowFiveInit :: rowFourInit :: rowFiveBlack :: rowFiveWhite :: rowFourWhite :: rowFourBlack:: Nil = Enum(20)
  val rowOne :: rowTwo :: rowThree :: rowFour :: rowFive :: Nil = Enum(5)
  val white :: black :: written :: Nil = Enum(3)
  val left :: right :: up :: down :: center :: rowFourFive :: Nil = Enum(6)
  val stateReg = RegInit(init)
  val innerState = RegInit(up)

  // Support registers
  val x = RegInit(0.U(32.W))
  val y = RegInit(0.U(32.W))
  val rowType = RegInit(0.U(32.W))
  val borderAdress = RegInit(0.U(32.W))
  val nextIsWhite = RegInit(false.B)
  val prevLine = Reg(Vec(18, UInt(2.W)))
  val thisLine = Reg(Vec(18, UInt(2.W)))
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
        stateReg := border
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
        innerState := center
        stateReg := blackCross
      } .otherwise {
        innerState := left
        stateReg := done
      }
    }
    is(blackCross) {
      blackFSMD()
    }
    is(border){
      borderFSMD()
    }
    is(specialRow){
      specialRowFSMD()
    }
    //skal rykkes ind i special row på en eller anden måde
    is(firstRow){
      io.address := 20.U * y + x + 400.U
      when(io.dataRead === 0.U){
        stateReg := rowBlack
      } .otherwise{
        stateReg := rowWhite
      }
    }
    is(lastRow){
      io.address := 20.U * y + x + 400.U
      when(io.dataRead === 0.U){
        stateReg := rowBlack
      } .otherwise{
        stateReg := rowWhite
      }
    }
    is(rowBlack){
      writeBlack(x,y)
      when(x >= 16.U){
          when(rowType === rowFive){
          x := 1.U
          stateReg := specialRow
          innerState := rowFourFive
          rowType := rowFive
          }.otherwise{
            x:=18.U
            stateReg := specialRow
            innerState := rowFourFive
            rowType := rowFour
          }
      }.otherwise{
        x := x + 5.U
        when(rowType === rowFive){
            stateReg := firstRow
          }.otherwise{
            stateReg := firstRow
          }
      }
      
    }
    is(rowWhite){
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
      updateRegisters()
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
            stateReg := specialRow
            innerState := rowFourFive
            rowType := rowFour
            x := 18.U
          } .otherwise {
            when(rowType === rowThree){
              stateReg := specialRow
              innerState := rowFourFive
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
  def updateRegisters(): Unit = {
    prevLine := thisLine
    thisLine := nextLine
    for (i <- 0 until 18) {
      nextLine(i) := 0.U
    }
  }

  //Seperate FSMD is a read pixel is black
  def blackFSMD(): Unit = {
    switch(innerState){
        is(center){
          writeBlack(x, y)
          when (x > 1.U){ //if we can go left
            innerState := left
          } .otherwise{
            when (x < 18.U){ //else if we can go right
              innerState := right
            } . otherwise{
              when (y > 1.U){ //else if we can go up
                innerState := up
              } .otherwise{
                when (y < 18.U){ //else if we can go down
                  innerState := down
                } .otherwise{
                  loop()
                }
              }
            }
          }
        }
        is(left){
          writeBlack(x - 1.U, y)
          when (x < 18.U){ //if we can go right
            innerState := right
          } .otherwise{
            when (y > 1.U){ //else if we can go up
              innerState := up
            } .otherwise{
              when (y < 18.U){ //else if we can go down
                innerState := down
              } .otherwise{
                innerState := center
                loop()
              }
            }
          }
        }
        is(right){
          writeBlack(x + 1.U, y)
            when (y > 1.U){ //else if we can go up
              innerState := up
            } .otherwise{
              when (y < 18.U){ //else if we can go down
                innerState := down
              } .otherwise{
                innerState := center
                loop()
              }
            }
        }
        is(up) {
          writeBlack(x,y - 1.U)
          when ( y < 18.U){
            innerState := down
          } .otherwise {
            innerState := center
            loop()
          }
        }
        is(down) {
          writeBlack(x,y + 1.U)
          innerState := center
          loop()
        }
      }
  }
  //Seperate FSMD is a read pixel is white
  def whiteFSMD(): Unit = {
    
  }

  def specialRowFSMD(): Unit = {
    switch(innerState){
      is(rowFourFive){
        when(rowType === rowFive){
          io.address := 20.U * y + 1.U
          when(io.dataRead === 0.U){
            innerState := black
          } .otherwise{
            innerState := white
          }
        }.otherwise{
          io.address := 20.U * y + 18.U
          when(io.dataRead === 0.U){
            innerState := black
          } .otherwise{
            innerState := white
          }
        }
      }
      is(black){
        when(rowType === rowFive){
          writeBlack(1.U,y)
          x := 5.U
          stateReg := forLoop
        }.otherwise{
          writeBlack(18.U,y)
          x := 4.U
          stateReg := forLoop
        }
      }
      is(white){

      }
    }
  }

  //Seperate FSMD for drawing border
  def borderFSMD(): Unit = {
    switch(innerState){
      is(up){
        writeBlack(x + 1.U,y)
        when (x === 18.U) {
          innerState := down
          x := 0.U
          y := 19.U
        } .otherwise {
          x := x + 1.U
        }
      }
      is(down){
        writeBlack(x + 1.U,y)
        when (x === 18.U) {
          x := 0.U
          y := 0.U
          innerState := left
        } .otherwise {
          x := x + 1.U
        }
      }
      is(left){
        writeBlack(x,y + 1.U)
        when (y === 18.U) {
          x := 19.U
          y := 0.U
          innerState := right
        } .otherwise {
          y := y + 1.U
        }
      }
      is(right){
        writeBlack(x,y + 1.U)
        when (y === 17.U) {
          x := 3.U
          y := 1.U
          rowType := rowFive
          stateReg := firstRow
        } .otherwise {
          y := y + 1.U
        }
      }
    }
  }
}
