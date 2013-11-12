package ar.edu.unq.tip.qsim.ui

import java.awt.Color
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable.Map
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
import ar.edu.unq.tpi.qsim.model.State._
import ar.edu.unq.tpi.qsim.model._
import ar.edu.unq.tpi.qsim.parser._
import ar.edu.unq.tpi.qsim.parser.ArquitecturaQ
import ar.edu.unq.tpi.qsim.utils._
import org.uqbar.arena.widgets.style.Style
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import org.uqbar.commons.utils.Observable
import org.uqbar.arena.windows.MessageBox
import org.uqbar.arena.bindings.NotNullObservable

class QSimWindow(owner: WindowOwner, model: QSimMain) extends Dialog[QSimMain](owner, model) {

  override def createErrorsPanel(parent: Panel)= {
    this.setTaskDescription("Agregue los archivos .qsim que desee ensamblar y luego cargar en memoria")
    super.createErrorsPanel(parent)
  }
  
  override def createFormPanel(mainPanel: Panel) = {
    this.setTitle("QSim")
    this.setIconImage("icon.png")
//    this.setIconImage(getClass().getResource("/icon.png").getPath())
    var form = new Panel(mainPanel)
    form.setLayout(new HorizontalLayout())
    var buttonPanel = new GroupPanel(form)
    buttonPanel.setTitle("Acciones")
    buttonPanel.setLayout(new VerticalLayout())
    
    new FileSelector(buttonPanel)
      .setCaption("Agregar")
      .bindValueToProperty("pathArchivo")
    new Button(buttonPanel).setCaption("Eliminar")
    	.onClick(new MessageSend(this.getModelObject(), "eliminarArchivo"))
    	.bindEnabled(new NotNullObservable("actual"))
    
    new Label(buttonPanel).setText("Seleccionar Arquitectura Q:")
    val arquitecturasQ = new Selector[ArquitecturaQ](buttonPanel)
	arquitecturasQ.setContents(Parser.arquitecturas, "name")		
	arquitecturasQ.bindValueToProperty("arquitecturaActual")
	
	new Button(buttonPanel).setCaption("Ensamblar")
      .onClick(new MessageSend(this.getModelObject(), "ensamblar"))
      .bindEnabled(new NotNullObservable("arquitecturaActual"))

     val w16Filter = new TextFilter() {
        def accept(event: TextInputEvent): Boolean = {
          event.getPotentialTextResult().matches("[A-F0-9]{0,4}")
        }
      }
      
    new Label(buttonPanel).setText("PC:")
    val pc = new TextBox(buttonPanel)
    pc.bindValueToProperty("pc") 
    pc.setWidth(110).setHeigth(15)
    pc.withFilter(w16Filter)
    
    new Label(buttonPanel).setText("Tamaño de memoria")
    val memoria = new TextBox(buttonPanel)
    memoria.bindValueToProperty("tamañoDeMemoria") 
    memoria.setWidth(110).setHeigth(15)
    memoria.withFilter(w16Filter)
    
    new Button(buttonPanel).setCaption("Cargar en memoria")
      .onClick(new MessageSend(this, "cargar"))
      .bindEnabled(new NotNullObservable("programa"))
    crearPanelDeEdicion(form)
  
  }

  def cargar() {
    val sim = new SimuladorAppmodel(model.programa, model.pc, Util.hexToInteger(model.tamañoDeMemoria))
    new QSimWindows(this, sim).open()
  }

  def crearPanelDeEdicion(parent: Panel) {
    var panelForm = new Panel(parent)
    panelForm.setLayout(new ColumnLayout(2))
    val list = new List(panelForm, "archivos")
    list.setWidth(200)
      .setHeigth(300)
      .bindValueToProperty("actual")

    val codeEditor = new KeyWordTextArea(panelForm)
    codeEditor.setWidth(300).setHeigth(300).bindValueToProperty("actual.codigo")
    codeEditor.keyWords("[a-z_]*") 
           .foreground(Color.MAGENTA).fontStyle(Style.ITALIC)
   codeEditor.keyWords("ADD","MOV","SUB","CALL","JAMP","RET", "MUL", "RET")
           .foreground(Color.BLUE).fontStyle(Style.BOLD)
   codeEditor.keyWords("""//[\w]+[\d]*\n?""")
           .foreground(Color.GREEN)
   codeEditor.keyWords("R[0-9]{1}")
           .foreground(Color.DARK_GRAY).fontStyle(Style.BOLD).fontStyle(Style.ITALIC)
   codeEditor.keyWords("0x[0-9A-F]{4}")
                   .foreground(Color.ORANGE)
   codeEditor.keyWords("[\\(\\)\\[\\]\\{\\}]")
                   .foreground(Color.DARK_GRAY).fontStyle(Style.BOLD)
  }
}
@Observable
class QSimMain {

  var archivos: java.util.List[Archivo] = scala.collection.immutable.List[Archivo]()
  var actual: Archivo = _
  var arquitecturaActual : ArquitecturaQ = Parser.arquitecturas(0)
  var programa: Programa = _
  var enabled = false
  var pc = "0000"
  var tamañoDeMemoria = "0300"
  
  def cambiarEnabled() {
    enabled = !enabled
  }

  def setPathArchivo(path: String) {
    var nombre = takeName(path)
    var codigo = readFile(path)
    var archivo = new Archivo(nombre, codigo)
    archivos = archivos.+:(archivo)
    println(archivos)
  }
  def getPathArchivo() = ""

  def readFile(path: String) = {
    val input = io.Source.fromFile(path)
    input.mkString
  }
  def eliminarArchivo(){
     archivos = archivos.-(actual)
     actual = null
  }
  def ensamblar() {
    programa = arquitecturaActual.parser(archivos.map(_.codigo).mkString)
  }

  def takeName(path: String) = {
    var part_path = path.split("/")
    part_path(part_path.length - 1)
  }
}

@Observable
class Archivo(var nombre: String, var codigo: String) {
  override def toString() = nombre
}

object laaa extends App {
  var la = new QSimMain()
  la.setPathArchivo("src/main/resources/programaQ1.qsim")
}
