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
import ar.edu.unq.tpi.qsim.utils._
import org.uqbar.arena.widgets.style.Style
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import org.uqbar.commons.utils.Observable
import ar.edu.unq.tpi.qsim.parser.Parser
import org.uqbar.arena.windows.MessageBox

class QSimWindow(owner: WindowOwner, model: QSimMain) extends Dialog[QSimMain](owner, model) {

  override def createErrorsPanel(parent: Panel)= {
    this.setTaskDescription("Agregue los archivos .qsim que desee ensamblar y luego cargar en memoria")
    super.createErrorsPanel(parent)
  }

  override def createFormPanel(mainPanel: Panel) = {
    this.setTitle("QSim")
    this.setIconImage(getClass().getResource("/icon.png").getPath())
    var form = new Panel(mainPanel)
    form.setLayout(new HorizontalLayout())
    var buttonPanel = new GroupPanel(form)
    buttonPanel.setTitle("")
    buttonPanel.setLayout(new VerticalLayout())
    new FileSelector(buttonPanel)
      .setCaption("Agregar")
      .bindValueToProperty("pathArcihvo")
    new Button(buttonPanel).setCaption("Eliminar")
    new Button(buttonPanel).setCaption("Ensamblar")
      .onClick(new MessageSend(this.getModelObject(), "ensamblar"))

    new Button(buttonPanel).setCaption("Cargar en memoria")
      .onClick(new MessageSend(this, "run"))


    crearPanelDeEdicion(form)
  }

 

  def run() {
    val sim = new SimuladorAppmodel(this.getModelObject().programa)
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
  // TODO el metodo quitar archivo de la lista

  var archivos: java.util.List[Archivo] = scala.collection.immutable.List[Archivo]()
  var actual: Archivo = _
  var programa: Programa = _
  var enabled = false
  
  def cambiarEnabled() {
    enabled = !enabled
  }

  def setPathArcihvo(path: String) {
    var nombre = takeName(path)
    var codigo = readFile(path)
    var archivo = new Archivo(nombre, codigo)
    archivos = archivos.+:(archivo)
    println(archivos)
  }
  def getPathArcihvo() = ""

  def readFile(path: String) = {
    val input = io.Source.fromFile(path)
    input.mkString
  }
  def ensamblar() {
    programa = Parser.ensamblarQ3SDFADSDFDSFASFASDFASDFASDFASD(archivos.map(_.codigo).mkString)
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
  la.setPathArcihvo("src/main/resources/programaQ1.qsim")
}
