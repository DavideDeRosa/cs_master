include "console.iol"
include "ui/swing_ui.iol"

main{
    showInputDialog@SwingUI("Insert your name:")(x.name)
    println@Console( x.name )() //wait for response
}