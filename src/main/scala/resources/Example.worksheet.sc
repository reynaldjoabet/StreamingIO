import iomonad.IO
import resources.Resources.Pull
import resources.Resources.Stream

Stream.resource(IO(1))(_ => IO.now(8).void).drain
