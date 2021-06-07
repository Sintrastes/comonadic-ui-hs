
module Control.UIFramework.Gtk where

import GI.Gtk.Declarative
import Control.UIFramework

-- Note: In order for this to work, I would need
-- to define my own widget type. The one for
-- gi-gtk-declaraitive is only generic in its
-- event type.

data GtkComponent window event a = GtkComponent {
  view  :: Bin window Widget event,
  state :: a
}

instance UIFramework IO GtkComponent where
--  runComponent component = undefined
