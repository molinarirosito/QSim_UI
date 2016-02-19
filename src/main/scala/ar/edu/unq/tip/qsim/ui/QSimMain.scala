package ar.edu.unq.tip.qsim.ui

/**
 * Copyright 2014 Tatiana Molinari.
 * Copyright 2014 Susana Rosito
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 */

import java.awt.Color

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

import org.uqbar.arena.actions.MessageSend
import org.uqbar.arena.bindings.NotNullObservable
import org.uqbar.arena.layout.ColumnLayout
import org.uqbar.arena.layout.HorizontalLayout
import org.uqbar.arena.layout.VerticalLayout
import org.uqbar.arena.widgets.Button
import org.uqbar.arena.widgets.FileSelector
import org.uqbar.arena.widgets.GroupPanel
import org.uqbar.arena.widgets.KeyWordTextArea
import org.uqbar.arena.widgets.Label
import org.uqbar.arena.widgets.List
import org.uqbar.arena.widgets.Panel
import org.uqbar.arena.widgets.Selector
import org.uqbar.arena.widgets.TextBox
import org.uqbar.arena.widgets.TextFilter
import org.uqbar.arena.widgets.TextInputEvent
import org.uqbar.arena.widgets.style.Style
import org.uqbar.arena.windows.Dialog
import org.uqbar.arena.windows.WindowOwner
import org.uqbar.commons.utils.Observable

import ar.edu.unq.tpi.qsim.model.Programa
import ar.edu.unq.tpi.qsim.parser.ArquitecturaQ
import ar.edu.unq.tpi.qsim.parser.Parser

class QSimWindow(owner: WindowOwner, model: QSimMain) extends Dialog[QSimMain](owner, model) {

  override def createErrorsPanel(parent: Panel) = {
    this.setTaskDescription("Agregue los archivos .qsim que desee ensamblar y luego cargar en memoria")
    super.createErrorsPanel(parent)
  }

  override def createFormPanel(mainPanel: Panel) = {
    this.setTitle("QSim")
    // De esta forma se tiene que poner cuando generamos el .jar
    this.setIconImage("icon.png")
    // De esta otra forma es para desarrollo
    // this.setIconImage(getClass().getResource("/icon.png").getPath())

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
    pc.setWidth(110).setHeight(15)
    pc.withFilter(w16Filter)

    //    new Label(buttonPanel).setText("Tamaño de memoria")
    //    val memoria = new TextBox(buttonPanel)
    //    memoria.bindValueToProperty("tamañoDeMemoria")
    //    memoria.setWidth(110).setHeigth(15)
    //    memoria.withFilter(w16Filter)

    new Button(buttonPanel).setCaption("Cargar en memoria")
      .onClick(new MessageSend(this, "cargar"))
      .bindEnabled(new NotNullObservable("programa"))
    crearPanelDeEdicion(form)
  }

  def cargar() {
    val sim = new SimuladorAppmodel(model.programa, model.pc)
    new QSimWindows(this, sim).open()
  }

  def crearPanelDeEdicion(parent: Panel) {
    var panelForm = new Panel(parent)
    panelForm.setLayout(new ColumnLayout(2))
    val list = new List(panelForm, "archivos")
    list.setWidth(200)
      .setHeight(300)
      .bindValueToProperty("actual")

    val codeEditor = new KeyWordTextArea(panelForm)
    codeEditor.setWidth(300).setHeight(300).bindValueToProperty("actual.codigo")
    codeEditor.keyWords("[a-z_]*")
      .foreground(Color.RED).fontStyle(Style.ITALIC)
    codeEditor.keyWords("MOV", "MUL", "SUB", "DIV", "ADD", "CALL", "RET", "CMP", "JMP", "JE", "JNE", "JLE", "JG", "JL", "JGE", "JLEU", "JGU", "JCS", "JNEG", "JVS")
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
  var arquitecturaActual: ArquitecturaQ = Parser.arquitecturas(5)
  var programa: Programa = _
  var enabled = false
  var pc = "0000"

  def cambiarEnabled() {
    enabled = !enabled
  }

  def setPathArchivo(path: String) {
    if (path != null) {
      var nombre = takeName(path)
      var codigo = readFile(path)
      println(codigo)
      var archivo = new Archivo(nombre, codigo)
      archivos = archivos.+:(archivo)
      println(archivos)
    }
  }

  def getPathArchivo() = ""

  def readFile(path: String) = {
    val input = io.Source.fromFile(path)
    input.mkString
  }
  def eliminarArchivo() {
    archivos = archivos.-(actual)
    actual = null
  }
  def ensamblar() {
    programa = null
    println("QUE ARQUITECTURA USAMOS \n" + arquitecturaActual)
    programa = arquitecturaActual.parser(archivos.map(_.codigo).mkString)
    println("RESULTADO DEL ENSAMBLADO \n" + programa)
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
  //  var program = args(0)
  //  var la = new QSimMain()
  //  la.setPathArchivo("src/main/resources/" + program)
  //  la.ensamblar()
}
