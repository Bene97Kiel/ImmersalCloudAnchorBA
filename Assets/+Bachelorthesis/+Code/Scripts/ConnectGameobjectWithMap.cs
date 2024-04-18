using System.Collections;
using System.Collections.Generic;
using UnityEngine;
namespace Immersal.AR
{
    public class ConnectGameobjectWithMap : MonoBehaviour
    {
        private GameObject _map;

        public string mapId;
        public string mapName;

        private string _nameOfMapGameobject;
        private bool firstLocalisation = true;


       
        // Start is called before the first frame update
        void Start()
        {
            _nameOfMapGameobject = "AR Map " + mapId + "-" + mapName;
            this.transform.localScale /= 1000;

        }

        // Update is called once per frame
        void Update()
        {


            if (GameObject.Find(_nameOfMapGameobject) == true && firstLocalisation)
            {
                _map = GameObject.Find(_nameOfMapGameobject);

                if (firstLocalisation)
                {
                    if (_map.transform.position.x != 0 )
                    {

                        this.transform.localScale *= 1000;

                        firstLocalisation = false;


                    }

                }

                this.transform.SetParent(_map.transform);
                Debug.Log(this.transform.parent.name);

                _map.GetComponent<ARMap>().renderMode = ARMap.RenderMode.EditorOnly;

                Debug.Log("Map was found " + _map.transform.position);

                
                
                

                Debug.Log("ObjectPosition " + this.transform.position);
            }

            
        }

    }
        
    }

    

