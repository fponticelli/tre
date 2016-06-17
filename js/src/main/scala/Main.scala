import org.denigma.threejs.{ PerspectiveCamera, Scene, WebGLRenderer, Box3, AxisHelper, CircleGeometry, Color, MeshLambertMaterial, MeshLambertMaterialParameters, AmbientLight, DirectionalLight, Mesh, Geometry }
import org.denigma.threejs.extensions.controls.{ CameraControls, JumpCameraControls, HoverControls }
import org.denigma.threejs.extras.{ TrackBallControls, HtmlSprite }
import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.raw.{ HTMLTextAreaElement, HTMLElement }

import tre.threejs.Convert
import tre.csg.Solid
import tre.csg.Solid._
import tre.csg.Resolution
import tre.d3._
import tre.d3.Point._
import tre.util.Measure

object Main extends scalajs.js.JSApp {
  val width  = 800
  val height = 600

  def makeWorld(): (PerspectiveCamera, Scene) = {
    val scene = new Scene()
    val camera = new PerspectiveCamera(45, width.toDouble / height, 0.1, 5000)
    val renderer = new WebGLRenderer
    renderer.setPixelRatio(dom.window.devicePixelRatio)
    renderer.setSize(width, height)
    renderer.setClearColor(new Color(0xFFFFFF))
    renderer.shadowMap.enabled = true
    renderer.antialias = true
    renderer.physicallyCorrectLights = true;
    renderer.gammaInput = true;
    renderer.gammaOutput = true;
    renderer.shadowMap.enabled = true;
    // renderer.toneMapping = THREE.ReinhardToneMapping;

    dom.document.body.appendChild(renderer.domElement)
    val controls = new TrackBallControls(camera) //, renderer.domElement)

    def onEnterFrame(f: Double): Unit = {
      controls.update()
      dom.window.requestAnimationFrame(onEnterFrame _)
      renderer.render(scene, camera)
    }
    onEnterFrame(0)

    (camera, scene)
  }

  def setupCamera(camera: PerspectiveCamera, scene: Scene): Unit = {
    val bBox = new Box3().setFromObject(scene)
    val width  = bBox.size().x
    val height = bBox.size().y
    val depth  = bBox.size().z
    val max = width max height max depth
    camera.position.set(-width/3*2, -max/2*3, height)
    camera.up.set(0, 0, 1)
    val c = bBox.center()
    scene.position.set(-c.x, -c.y, -c.z)
    val axis = new AxisHelper(max)
    scene.add(axis)
    // ground
    val multiplier = 4
    val groundGeo = new CircleGeometry(max * multiplier, 32)
    val groundMat = new MeshLambertMaterial(js.Dynamic.literal(
          color = new Color(0xFFFFFF)
        ).asInstanceOf[MeshLambertMaterialParameters])
    val ground = new Mesh(groundGeo, groundMat)
    ground.position.x = c.x
    ground.position.y = c.y
    ground.position.z = bBox.min.z
    ground.receiveShadow = true;
    scene.add(ground)

    var ambient = new AmbientLight(0xFFFFFF)
    scene.add(ambient)

    // lights
    val light1 = directionLight(0xFFFFFF, -3, -2, 5)
    scene.add(light1)
    // scene.add(new CameraHelper(light1.shadow.camera))

    // val light2 = directionLight(0xFFFFFF, 2, -1, 3)
    // scene.add(light2)
    // scene.add(new CameraHelper(light2.shadow.camera))

    // scene.fog = new Fog(0xFFFFFF, 1, max * multiplier * 3)
  }

  def directionLight(color: Int, x: Double, y: Double, z: Double): DirectionalLight = {
    val light = new DirectionalLight(color, 0.75)
    light.position.set(x, y, z)
    light.castShadow = true
    light.shadow.bias = -0.00000001
    light.shadow.mapSize.x = 2048
    light.shadow.mapSize.y = 2048
    light
  }

  def main(): Unit = {
    val (camera, scene) = makeWorld()

    val material = new MeshLambertMaterial(js.Dynamic.literal(
          color = new Color(0x888888)
        ).asInstanceOf[MeshLambertMaterialParameters])
    val m = Measure.time(buildSolids)
    var g = buildGeometries(m.computation)
    println(s"CSG computation took: ${m.value / 1000000000.0}s")
    g.foreach((g: Geometry) => scene.add({
      val mesh = new Mesh(g, material)
      mesh.receiveShadow = true
      mesh.castShadow = true
      mesh
    }))

    setupCamera(camera, scene)
  }

  def buildGeometries(xs: Vector[Solid]): Vector[Geometry] =
    xs.map(Convert.solid2geometry)

  def buildSolids(): Vector[Solid] = {
    implicit val res = Resolution.byFeature(0.08)
    val b = cube((-0.5,-0.5,-0.5), 1.0) +
            cube((0.1,0.1,0.1), 1.0) +
            cube((-1.1,-1.1,-1.1), 1.0) ^
            sphere((0.0,0.0,0.0), 0.7) -
            sphere((-0.5,-0.5,-0.5), 0.35) -
            cylinder((0.0,0.0,-0.95), (0.0,0.0,0.95), 0.25) -
            cylinder((0.0,-0.95,0.0), (0.0,0.95,0.0), 0.35) -
            cylinder((-0.95,0.0,0.0), (0.95,0.0,0.0), 0.15)
    Vector(b)
  }
}
