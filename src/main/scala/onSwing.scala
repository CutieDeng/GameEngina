package engine

import java.awt.{BasicStroke, Color, Graphics, Graphics2D, RenderingHints, Stroke}
import java.awt.event.{MouseEvent, MouseListener, MouseMotionListener}
import java.awt.geom.{Arc2D, Area, Ellipse2D}
import java.util
import javax.swing.border.StrokeBorder
import javax.swing.{JButton, JComponent, JFrame, JPanel, JSpinner, SpinnerListModel, Spring, SpringLayout, Timer, WindowConstants}

@main
def onSwing() : Unit = {
    
    val frame = JFrame()
    val panel = frame.getContentPane
    val rootLayout = SpringLayout()
    panel.setLayout(rootLayout)
    
    val but0 = JButton()
    but0.setText("Button 1")
    panel.add(but0)
    rootLayout.putConstraint(SpringLayout.WEST, but0, 10, SpringLayout.WEST, panel)
    
    val but1 = JButton()
    but1.setText("Button 2")
    panel.add(but1)
    rootLayout.putConstraint(SpringLayout.WEST, but1, 10, SpringLayout.WEST, panel)
    rootLayout.putConstraint(SpringLayout.VERTICAL_CENTER, but1, 40, SpringLayout.NORTH, panel)
    
    val b0 = rootLayout.getConstraints(but0)
    val s0 = Spring.scale(rootLayout.getConstraint(SpringLayout.WIDTH, panel), 0.5)
    b0.setWidth(s0)
    b0.setHeight(Spring.constant(20, 30, 35))
    
    val jSpinner = JSpinner()
    val heys = Seq("I Love hyh", "EMPTY", "null", "economy")
    val list = util.ArrayList[String]()
    for (h <- heys) list.add(h)
    val model = SpinnerListModel()
    jSpinner.setModel(model)
    model.setList(list)
    rootLayout.putConstraint(SpringLayout.NORTH, jSpinner, 10, SpringLayout.SOUTH, but1)
    rootLayout.putConstraint(SpringLayout.WEST, jSpinner, 15, SpringLayout.WEST, panel)
    // panel.add(jSpinner)
    
    val circleToolTip = new JComponent with MouseListener with MouseMotionListener {
        
        override def mouseDragged(e: MouseEvent): Unit = {}
        
        override def mouseMoved(e: MouseEvent): Unit = {
            (mouseX, mouseY, activated) match {
                case (x: Int, y: Int, true) => {
                    val mouseX = e.getX
                    val mouseY = e.getY
                    val diffX = mouseX - x
                    val diffY = mouseY - y
                    val tan = diffX.doubleValue / diffY.doubleValue
                    var angular = Math.atan(tan)
                    if (diffY < 0) {
                        angular += Math.PI
                    }
                    angular -= Math.PI / selections.length
                    if (angular < 0) {
                        angular += Math.PI * 2
                    }
                    val id = angular / (Math.PI * 2 / selections.length)
                    val idInt = id.intValue
                    if (this.selectId != idInt) {
                        this.selectId = idInt
                        this.repaint()
                    }
                }
                case _ => {}
            }
        }
        
        val MAX_RADIUS : Int = 50
        
        val timer: Timer = javax.swing.Timer(20, { e =>
            (this.mouseX, this.mouseY, this.radius) match {
                case (x: Int, y: Int, r: Int) => {
                    if (r < MAX_RADIUS) {
                        this.radius = r + 6
                        this.repaint()
                    } else {
                        this.timer.stop()
                    }
                }
            }
        })
        @volatile
        var mouseX: Int | Null = null
        @volatile
        var mouseY: Int | Null = null
        @volatile
        var radius: Int | Null = null
        @volatile
        var activated : Boolean = false
        @volatile
        var selectId : Int | Null = null
        
        var selections : Vector[(() => Unit, Color)] = {
            val i : (() => Unit, Color) = ({ () => }, Color.GRAY)
            Vector.empty.appended(i).appended(i).appended(i).appended(i)
        }
        
        override def paint(g: Graphics): Unit = {
            super.paintComponent(g)
            (this.mouseX, this.mouseY, this.radius, this.activated) match {
                case (x: Int, y: Int, r: Int, true) => {
                    val graphics2D = g.asInstanceOf[Graphics2D]
                    graphics2D.setStroke(BasicStroke(2.0))
                    graphics2D.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
                    var c = Color.DARK_GRAY.getRGB
                    c &= 0x00ffffff
                    c |= 0x80000000
                    // graphics2D.setColor(Color(c, true))
                    // graphics2D.fillOval(x - r, y - r, 2 * r, 2 * r)
                    graphics2D.setColor(Color.LIGHT_GRAY)
                    graphics2D.drawOval(x - r, y - r, 2 * r, 2 * r)
                    if (r >= MAX_RADIUS) {
                        if (selections.isEmpty) {
                            graphics2D.drawOval(x - 10, y - 10, 20, 20)
                            val fontMetrics = graphics2D.getFontMetrics
                            val toolTip = "Hello"
                            val xToolTip = x - fontMetrics.stringWidth(toolTip) / 2
                            graphics2D.drawString(toolTip, xToolTip, y)
                        } else {
                            val outer: Area = Area(Ellipse2D.Double(x - r, y - r, 2 * r, 2 * r))
                            val inner: Area = Area(Ellipse2D.Double(x - r / 2, y - r / 2, r, r))
                            outer.subtract(inner)
                            for (i <- selections.indices) {
                                val arc : Arc2D.Float = Arc2D.Float(Arc2D.PIE)
                                arc.setFrame(x - r, y - r, 2 * r, 2 * r)
                                arc.setAngleStart(-45 + 90 * i)
                                arc.setAngleExtent(90)
                                val ar = Area(arc)
                                ar.intersect(outer)
                                selectId match {
                                    case id: Int if i == id => {
                                        graphics2D.setColor(Color.GRAY)
                                    }
                                    case _ => {
                                        graphics2D.setColor(Color.LIGHT_GRAY.brighter())
                                    }
                                }
                                graphics2D.fill(ar)
                                graphics2D.setColor(Color.BLACK)
                                graphics2D.draw(ar)
                            }
                        }
                    }
                }
                case _ => {}
            }
        }
        
        override def mouseClicked(e: MouseEvent): Unit = {
            if (e.getButton == MouseEvent.BUTTON3) {
                mouseX = e.getX
                mouseY = e.getY
                radius = 0
                activated = true
                // handle it
                timer.start()
            } else if (e.getButton == MouseEvent.BUTTON1) {
                activated = false
                // check
                this.selectId match {
                    case _ => {
                    
                    }
                }
                this.repaint()
            }
        }
        
        override def mousePressed(e: MouseEvent): Unit = {}
        
        override def mouseReleased(e: MouseEvent): Unit = {}
        
        override def mouseEntered(e: MouseEvent): Unit = {}
        
        override def mouseExited(e: MouseEvent): Unit = {
            // do something here
            activated = false
            repaint()
        }
    }
    
    rootLayout.putConstraint(SpringLayout.WEST, circleToolTip, 0, SpringLayout.WEST, panel)
    rootLayout.putConstraint(SpringLayout.EAST, circleToolTip, 0, SpringLayout.EAST, panel)
    rootLayout.putConstraint(SpringLayout.NORTH, circleToolTip, 0, SpringLayout.NORTH, panel)
    rootLayout.putConstraint(SpringLayout.SOUTH, circleToolTip, 0, SpringLayout.SOUTH, panel)
    panel.add(circleToolTip)
    panel.addMouseListener(circleToolTip)
    panel.addMouseMotionListener(circleToolTip)
    panel.setComponentZOrder(circleToolTip, 0)
    
    frame.pack()
    frame.setSize(600, 400)
    frame.setVisible(true)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
}
