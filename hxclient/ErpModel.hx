package;

import haxe.ui.toolkit.core.Macros;
import haxe.ui.toolkit.core.Toolkit;
import haxe.ui.toolkit.core.Root;
import haxe.ui.toolkit.controls.Button;
import haxe.ui.toolkit.events.UIEvent;

class Main {
	package static function main () : void {
		Macros.addStylesheet("styles/gradient/gradient.css")
		Toolkit.init()
		Toolkit.openFullScreen (
			function(root:Root){
				var button : Button = new Button();
				button.text = "Help!!"
				button.x = 100
				button.y = 100
				button.onClick = function(e: UIEvent){
					e.component.text = "I was clicked"
				}
			};
		root.addChild(button)
		));
	}

}