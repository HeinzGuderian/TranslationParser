(in-package :csharp-parser) 

(declaim (optimize (debug 3)))

;; (in-package :csharp-parser)
;; (defparameter test (parse-csharp (tokenize-csharp-code *code-test-class*)))

(defparameter *code-test* 
"public void Start(container as List){
     return \"ArmyEconomy\";} ")

(defparameter *code-test-class* 
" 
using UnityEngine;
using UnityEngine;

partial public class FactoryEconomy : BuildingEconomy, IGUI {}"
)

(defparameter *code-test-variables-simple* 
" 
using UnityEngine;
using UnityEngine;

partial public class FactoryEconomy : BuildingEconomy, IGUI 
{
 int c;
 private int a;
 private int b = 2;
}"
)

(defparameter *code-test-variables-advanced* 
" 
using UnityEngine;
using UnityEngine;

partial public class FactoryEconomy : BuildingEconomy, IGUI {
      public GameObject _playerGameObject;
      TeamScript.PlayerNumberEnum _winningPlayer;
      private int[] d;
}
")

"

      private List<int> _playerGameObjectList;
      bool _haveConceded = false;
      private int a = 3;
      private int b = 3;
      var b = 2;
"
