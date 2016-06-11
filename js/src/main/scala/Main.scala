import org.denigma.threejs._
import org.denigma.threejs.extensions.Container3D
import org.denigma.threejs.extensions.controls.{ CameraControls, JumpCameraControls, HoverControls }
import org.denigma.threejs.extras.HtmlSprite
import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.raw.{ HTMLTextAreaElement, HTMLElement }

import tre.threejs.Convert._
import tre.csg._
import tre.d3._

object Main extends scalajs.js.JSApp {
  val width  = 600
  val height = 400

  def makeWorld(): Scene = {
    dom.console.log(dom.window.devicePixelRatio)
    val scene = new Scene()
    val camera = new PerspectiveCamera(45, width.toDouble / height, 0.1, 1000)
    val renderer = new WebGLRenderer
    renderer.setPixelRatio(dom.window.devicePixelRatio)
    renderer.setSize(width, height)
    renderer.setClearColor(new Color(0xf6f6f6))
    dom.document.body.appendChild(renderer.domElement)
    val controls: CameraControls = new HoverControls(camera, renderer.domElement)

    val light1 = new DirectionalLight(0xffffff, 2)
    light1.position.set(2, 1, 3).normalize()
    scene.add(light1)

    val light2 = new DirectionalLight(0x999999, 2)
    light2.position.set(-2, -1, 2).normalize()
    scene.add(light2)

    camera.position.z = 5

    def onEnterFrame(f: Double): Unit = {
      controls.update()
      dom.window.requestAnimationFrame(onEnterFrame _)
      renderer.render(scene, camera)
    }
    onEnterFrame(0)

    scene
  }

  def main(): Unit = {
    val scene = makeWorld()

    val material = new MeshLambertMaterial(js.Dynamic.literal(
          color = new Color(0x505050)
        ).asInstanceOf[MeshLambertMaterialParameters])
    buildGeometries.foreach((g: Geometry) => scene.add(new Mesh(g, material)))
  }

  def buildGeometries(): List[Geometry] = {
    val b1 = Solid.box(Point(-0.0, -0.0, 0), Point(1, 1, 1))
    val b2 = Solid.box(Point(-0.5, -0.5, -0.2), Point(1, 1, 1))
    List(b1, b2)
  }
}
