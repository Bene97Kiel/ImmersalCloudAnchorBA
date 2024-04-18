Ausführen der für die Thesis genutzten ARCloud Scene:


Relevante Dateien:

Scene: Mapping App; Thesis_ARCloud_Scene
Ordner: Assets/+Bachelorthesis/+Level/ThesisScenes

Prefabs: Kerzen; InfoCanvas; 3D_Icon; Cube
Ordner: Assets/+Bachelorthesis/+Level/ThesisPrefabs

Script: ConnectGameObjectMitMap
Ordner: Assets/+Bachelorthesis/+Scripts



1. Anlegen eines Immersal Developers Account
https://developers.immersal.com/index.html

2. Mapping App bauen und ausführen um eine Map aufzunehmen
Scene: Mapping App
Ordner: Assets/+Bachelorthesis/+Level/ThesisScenes

3. Developer Token, Map ID und Name aus Immersal Cloud Console herauslesen
https://developers.immersal.com/index.html

4. Thesis_AR_Cloud_Scene öffnen und Developer Token im ImmersalSDK-GameObject einfügen
Scene: Thesis_ARCloud_Scene
Ordner: Assets/+Bachelorthesis/+Level/ThesisScenes

5. In der ARLocalizer-Komonente des ImmersalSDK-Gameobjects Map ID hinterlegen

6. AR_Inhalt in die Scene einfügen und das Script ConnectGameObjectMitMap an AR_Inhalt anfügen und MapId und MapName in die public Varaible des Scripts eintragen
Script: ConnectGameObjectMitMap
Ordner: Assets/+Bachelorthesis/+Script

!!ACHTUNG!! Das GameObject "AR Space" in der Hierarchy muss für den Build auf inaktiv gesetzt sein


7. Bauen und ausführen der Thesis_AR_Cloud Scene 





