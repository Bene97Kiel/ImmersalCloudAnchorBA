using System.Collections;
using System.Collections.Generic;
using UnityEngine;
namespace Immersal.AR
{
    public class PersoImageToCanvas : MonoBehaviour
    {
        private GameObject _map;

        public GameObject destinationObject1;

        public string mapId;
        public string mapName;

        private string _nameOfMapGameobject;
        private bool firstLocalisation = true;

        private Camera mainCamera;
        private Transform objectToMove;
        private Vector3 targetPosition;
        public float moveDuration = 2f;
        private Quaternion targetRotation;
        public float rotationSpeed = 5f;
        private Vector3 originalPosition;
        private Quaternion originalRotation;

        private int touchIndex = 0;


        // Start is called before the first frame update
        void Start()
        {
            _nameOfMapGameobject = "AR Map " + mapId + "-" + mapName;
            this.transform.localScale /= 1000;
            mainCamera = Camera.main;
            objectToMove = transform;


        }

        // Update is called once per frame
        void Update()
        {


            if (GameObject.Find(_nameOfMapGameobject) == true && firstLocalisation)
            {
                _map = GameObject.Find(_nameOfMapGameobject);

                if (firstLocalisation)
                {
                    if (_map.transform.position.x != 0)
                    {

                        this.transform.localScale *= 1000;

                        firstLocalisation = false;
                        originalPosition = objectToMove.position;
                        originalRotation = objectToMove.rotation;


                    }

                }

                this.transform.SetParent(_map.transform);
                Debug.Log(this.transform.parent.name);

                _map.GetComponent<ARMap>().renderMode = ARMap.RenderMode.EditorOnly;

                Debug.Log("Map was found " + _map.transform.position);





                Debug.Log("ObjectPosition " + this.transform.position);
            }

            if (Input.touchCount > 0 && Input.GetTouch(0).phase == TouchPhase.Began)
            {
                // Get the touch position
                Vector2 touchPosition = Input.GetTouch(0).position;

                // Cast a ray from the touch position
                RaycastHit hit;
                Ray ray = mainCamera.ScreenPointToRay(touchPosition);

                // Check if the ray hits the object
                if (Physics.Raycast(ray, out hit))
                {
                    if (hit.collider.gameObject == gameObject)
                    {
                        touchIndex++;
                        Debug.Log("Hit detetcted with touchindex:" + touchIndex);
                        if (touchIndex % 2 == 1)
                        {
                            StartCoroutine(MoveObjectAfterDelay(0f));

                        }
                        else
                        {

                            StopAllCoroutines(); // Stop any ongoing animations
                            StartCoroutine(MoveToObjectPosition(destinationObject1.transform.position,destinationObject1.transform.rotation, 0f));
                        }


                    }
                }
            }
        }

        IEnumerator MoveObjectAfterDelay(float delay)
        {
            yield return new WaitForSeconds(delay);

            // Calculate a position in front of the camera
            Vector3 cameraForward = mainCamera.transform.forward;
            targetPosition = mainCamera.transform.position + cameraForward * 0.5f; // Move 5 units in front of the camera
            //targetPosition.y = objectToMove.position.y; // Maintain the same height as the original object
            targetRotation = Quaternion.LookRotation(mainCamera.transform.forward, Vector3.up);

            // Move the object
            StartCoroutine(AnimateMovement());

        }

        IEnumerator AnimateMovement()
        {
            float elapsedTime = 0f;
            Vector3 startPosition = objectToMove.position;

            while (elapsedTime < moveDuration)
            {
                // Calculate the interpolation ratio
                float t = elapsedTime / moveDuration;
                // Lerp between the start and target positions
                objectToMove.position = Vector3.Lerp(startPosition, targetPosition, t);
                // Increment the elapsed time
                elapsedTime += Time.deltaTime;
                yield return null;
            }

            // Ensure final position
            objectToMove.position = targetPosition;
            while (Quaternion.Angle(objectToMove.rotation, targetRotation) > 0.1f)
            {
                // Rotate the object towards the target rotation
                objectToMove.rotation = Quaternion.Slerp(objectToMove.rotation, targetRotation, rotationSpeed * Time.deltaTime);
                yield return null;
            }

            // Ensure final rotation
            objectToMove.rotation = targetRotation;
        }



        IEnumerator MoveObjectToOriginalPosition()
        {
            float elapsedTime = 0f;
            Vector3 startPosition = objectToMove.position;
            Quaternion startRotation = objectToMove.rotation;

            while (elapsedTime < moveDuration)
            {
                // Calculate the interpolation ratio
                float t = elapsedTime / moveDuration;
                // Lerp between the current position and the original position
                objectToMove.position = Vector3.Lerp(startPosition, originalPosition, t);
                // Slerp between the current rotation and the original rotation
                objectToMove.rotation = Quaternion.Slerp(startRotation, originalRotation, t);
                // Increment the elapsed time
                elapsedTime += Time.deltaTime;
                yield return null;
            }

            // Ensure final position and rotation
            objectToMove.position = originalPosition;
            objectToMove.rotation = originalRotation;
        }

        IEnumerator MoveToObjectPosition(Vector3 targetPosition, Quaternion targetRotataion, float delay)
        {
            yield return new WaitForSeconds(delay);

            float elapsedTime = 0f;
            Vector3 startPosition = objectToMove.position;
            Quaternion startRotation = objectToMove.rotation;


            while (elapsedTime < moveDuration)
            {
                // Calculate the interpolation ratio
                float t = elapsedTime / moveDuration;
                // Lerp between the current position and the target position
                objectToMove.position = Vector3.Lerp(startPosition, targetPosition, t);
                objectToMove.rotation = Quaternion.Slerp(startRotation, targetRotataion, t);

                // Increment the elapsed time
                elapsedTime += Time.deltaTime;
                yield return null;
            }

            // Ensure final position
            objectToMove.position = targetPosition;
        }
    }

}



