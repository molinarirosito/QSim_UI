package ar.edu.unq.tip.qsim.ui

import java.awt.Color

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable.Map
import org.uqbar.arena.windows.MessageBox
import org.apache.commons.lang.StringUtils
import org.uqbar.arena.Application
import org.uqbar.arena.actions.MessageSend
import org.uqbar.arena.bindings.ObservableProperty
import org.uqbar.arena.layout.{ ColumnLayout, HorizontalLayout, VerticalLayout }
import org.uqbar.arena.widgets._
import org.uqbar.arena.widgets.tables.Column
import org.uqbar.arena.widgets.tables.Table
import org.uqbar.arena.windows.{ Dialog, Window, WindowOwner }
import org.uqbar.commons.utils.{ Observable, ReflectionUtils, When }
import com.uqbar.commons.collections.Transformer
import org.uqbar.arena.widgets.style.Style
import ar.edu.unq.tpi.qsim.model.State._
import ar.edu.unq.tpi.qsim.model._
import ar.edu.unq.tpi.qsim.utils._

@Observable
class Fila16Celdas(aName: String) extends Fila4Celdas(aName) {
  var celda4, celda5, celda6, celda7, celda8, celda9, celda10, celda11, celda12, celda13, celda14, celda15: Celda = _
}

@Observable
case class Fila4Celdas(name: String) {
  var celda0, celda1, celda2, celda3: Celda = _

  def apply(indice: Int, celda: Celda) {
    val field = ReflectionUtils.getField(this.getClass(), s"celda$indice")
    field.setAccessible(true)
    field.set(this, celda)
    When(celda) change "value" and "state" fireChange s"celda$indice" of this
  }
}

object SimuladorAppmodel{
	var instrucciones = scala.List(
			ADD(R0, Inmediato("0002")),
			ADD(R1, Inmediato("00AF")),
			MOV(Directo(Inmediato("FFFD")), Inmediato("FFFF")),
			MOV(Directo(Inmediato("0040")), Inmediato("AAAA")),
			MOV(Directo(Inmediato("0001")), Inmediato("2222")),
			MOV(Directo(Inmediato("0001")), Inmediato("9999")),
			ADD(R0, Inmediato("0002")),
			MUL(R4, Inmediato("0001")),
			MOV(R5, Inmediato("0056")),
			SUB(R5, Inmediato("000A")))
}

@Observable
class SimuladorAppmodel(programa: Programa = new Programa(SimuladorAppmodel.instrucciones)) {
  var enabled = false
  // var programa = new Programa(instrucciones)
  var sim = Simulador()
  sim.inicializarSim()
  var celdas = new java.util.ArrayList[Fila16Celdas]()
  
  crearFila16Celdas()
  sim.cargarProgramaYRegistros(programa, "0000", Map[String, W16]())

  def crearFila16Celdas() {
    var contador = 0
    val memoria = sim.busIO.memoria
    var name = new W16("0000")
    var row = 0
    var fila: Fila16Celdas = new Fila16Celdas(name.toString)
    do {
      if (row >= 16) {
        row = 0
        celdas.append(fila)
        name = name.+(new W16("0010"))
        fila = new Fila16Celdas(name.toString)
      }
      fila(row, memoria.celda(contador))
      contador = contador + 1
      row = row + 1
    } while (contador < memoria.tamanioMemoria())
  }

  

  def cambiarEdicion() {
    enabled = !enabled
  }

}

class QSimWindows(owner: WindowOwner, model: SimuladorAppmodel) extends Dialog[SimuladorAppmodel](owner, model) {

   override def createErrorsPanel(parent: Panel)= {
    this.setTaskDescription("")
    super.createErrorsPanel(parent)
  }
  
   
  override def createFormPanel(mainPanel: Panel) = {
    this.setTitle("Qsim")
    this.setIconImage(getClass().getResource("/icon.png").getPath())
    var groupPanel = new Panel(mainPanel)
    				.setLayout(new VerticalLayout())
    var form = new Panel(groupPanel)
    form.setLayout(new HorizontalLayout())

    var second_form = new Panel(form)
    second_form.setLayout(new VerticalLayout())
    crearRegistrosEspeciales(second_form)
    crearFlags(second_form)
    crearPanelDeRegistros(second_form)
    crearMemoria(form)
    crearTexArea(second_form)
  }

  def crearMemoria(parent: Panel) {
    var memoriaForm = new Panel(parent)
    memoriaForm.setLayout(new ColumnLayout(1))

    var table = new Table[Fila16Celdas](parent, classOf[Fila16Celdas])
    table.setHeigth(500)
    table.setWidth(850)
    table.bindItemsToProperty("celdas")
    new Column[Fila16Celdas](table) //
      .setTitle("")
      .setFixedSize(50)
      .bindContentsToProperty("name")
    for (celdas <- 0 to 15) {
      val column = new Column[Fila16Celdas](table) //
      column.setTitle(Util.IntSumToHex(celdas))
        .setFixedSize(50)
        .bindContentsToProperty(s"celda$celdas")
      column.bindBackground(s"celda$celdas.state", new Transformer[Type, Color]() {
        def transform(element: Type) = element match {
          case NONE => Color.WHITE
          case PROGRAM => Color.LIGHT_GRAY
          case STORE => Color.BLUE
          case FECH_DECODE => Color.GREEN
          case EXECUTED => Color.CYAN
          case _ => null
        }
      })
    }

  }
  
  def crearRegistrosEspeciales(parent: Panel) {
    var FlagsForm = new GroupPanel(parent)
    FlagsForm.setTitle("Registros especiales")
    FlagsForm.setLayout(new ColumnLayout(2))
    
    new Label(FlagsForm).setText("PC")
      val text_pc = new TextBox(FlagsForm)
      text_pc.bindEnabledToProperty("enabled")
      text_pc.bindValueToProperty(s"sim.cpu.pc.hex")
      text_pc.setWidth(110).setHeigth(15)
      
      new Label(FlagsForm).setText("SP")
      val text_sp = new TextBox(FlagsForm)
      text_sp.bindEnabledToProperty("enabled")
      text_sp.bindValueToProperty(s"sim.cpu.sp")
      text_sp.setWidth(110).setHeigth(15)
      
      new Label(FlagsForm).setText("IR")
      val text = new TextBox(FlagsForm)
      text.bindEnabledToProperty("enabled")
      text.bindValueToProperty(s"sim.cpu.ir")
      text.setWidth(110).setHeigth(15)
    
  }
  
  def crearFlags(parent: Panel) {
    var FlagsForm = new GroupPanel(parent)
    FlagsForm.setTitle("Flags")
    FlagsForm.setLayout(new ColumnLayout(4))
   
      
      
      scala.List[String]("n", "v", "z", "c").foreach(prop => {
      new Label(FlagsForm).setText(prop.toUpperCase)
      val text = new TextBox(FlagsForm)
      text.bindEnabledToProperty("enabled")
      text.bindValueToProperty(s"sim.cpu.$prop")
      text.setWidth(30).setHeigth(15)
      text.withFilter(new TextFilter() {
        def accept(event: TextInputEvent): Boolean = {
          event.getPotentialTextResult().matches("[0-1]{0,1}")
        }
      })
    })
    
  }

  def crearTexArea(parent: Panel) {
	    var panelForm = new Panel(parent)
	    panelForm.setLayout(new HorizontalLayout())
	     new TextBox(panelForm)
	    	.setMultiLine(true)
	    	.selectFinalLine()
	    	.setWidth(360)
	    	.setHeigth(220)
	    	.bindValueToProperty("sim.mensaje_al_usuario")
  }

  def crearPanelDeRegistros(parent: Panel) {
    var registrosForm = new GroupPanel(parent)
    registrosForm.setTitle("Registros")
    registrosForm.setLayout(new ColumnLayout(4))
    model.sim.cpu.registros.foreach(registro => {
      val registroPanel = new Panel(registrosForm, registro).setLayout(new ColumnLayout(2))
      new Label(registroPanel).setText("R" + registro.numero)
      val text = new TextBox(registroPanel)
      text.withFilter(new TextFilter() {
        def accept(event: TextInputEvent): Boolean = {
          event.getPotentialTextResult().matches("[A-F0-9]{0,4}")
        }
      })
      text.bindEnabled(new ObservableProperty(this.getModelObject(), "enabled"))
      text.bindValueToProperty("valor.hex")
      text.bindBackgroudToTransformer("cambio", new Transformer[Boolean, Color]() {
        def transform(cambio: Boolean) = if (cambio) Color.BLUE else Color.WHITE
      })
    })
  }

  override def addActions(actions: Panel) = {
    var panelForm = new Panel(actions)
    panelForm.setLayout(new ColumnLayout(2))
    
    var ciclo_Form = new GroupPanel(panelForm)
    ciclo_Form.setTitle("Ciclo ejecucion")
    ciclo_Form.setLayout(new HorizontalLayout())
    
    scala.List("fetch", "decode", "execute").foreach(action => {
      new Button(ciclo_Form)
        .setCaption(StringUtils.capitalize(action))
        .onClick(new MessageSend(model.sim, action))
        .setAsDefault
        .bindEnabled(new ObservableProperty(model.sim.ciclo, action))
    })   
    
    
    var buttons_Form = new GroupPanel(panelForm)
    buttons_Form.setTitle("")
    buttons_Form.setLayout(new HorizontalLayout())

    val editable = new Button(buttons_Form)
      .setCaption("Editable")
      .onClick(new MessageSend(model, "cambiarEdicion"))
      .setAsDefault

    val ver_puertos =new Button(buttons_Form)
      .setCaption("Ver puertos")
      .onClick(new MessageSend(QSimWindows.this, "createPuertosWindow"))
      .setAsDefault

  }

  def createPuertosWindow() = {
    (new PuertosWindow(this.getOwner(), this.getModelObject())).open()
  }

}

object QSimRunner extends Application with App {

  def createMainWindow(): Window[_] = {
        val sim = new SimuladorAppmodel()
        new QSimWindows(this, sim)

    var la = new QSimMain()
  la.setPathArcihvo("src/main/resources/programaQ1.qsim")
   la.setPathArcihvo("src/main/resources/programaQ2.qsim")
    la.setPathArcihvo("src/main/resources/programaQ3.qsim")
    new QSimWindow(this, la)
  }
  start()
}
