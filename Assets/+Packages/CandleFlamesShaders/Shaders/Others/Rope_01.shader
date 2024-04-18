// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "CandleFlamesShaders/Others/Rope_01"
{
	Properties
	{
		_Color("Color", 2D) = "white" {}
		_Normal("Normal", 2D) = "bump" {}
		_NormalScale("NormalScale", Range( 0 , 1)) = 1
		[HDR]_EmissionColor("EmissionColor", Color) = (1,1,1,0)
		_Emission_min("Emission_min", Range( -1 , 1)) = 1
		_Emission_max("Emission_max", Range( -1 , 1)) = 1
		_Emission_hard("Emission_hard", Float) = 5
		[HideInInspector] _texcoord( "", 2D ) = "white" {}
		[HideInInspector] __dirty( "", Int ) = 1
	}

	SubShader
	{
		Tags{ "RenderType" = "Opaque"  "Queue" = "Geometry+0" "IsEmissive" = "true"  }
		Cull Back
		CGPROGRAM
		#include "UnityStandardUtils.cginc"
		#pragma target 3.0
		#pragma surface surf Standard keepalpha addshadow fullforwardshadows 
		struct Input
		{
			float2 uv_texcoord;
		};

		uniform float _NormalScale;
		uniform sampler2D _Normal;
		uniform float4 _Normal_ST;
		uniform sampler2D _Color;
		uniform float4 _Color_ST;
		uniform float _Emission_min;
		uniform float _Emission_max;
		uniform float _Emission_hard;
		uniform float4 _EmissionColor;


		float Sigmoid2_g1( float x , float y )
		{
			return 1 / (1 + pow(2.718, -x * y));
		}


		void surf( Input i , inout SurfaceOutputStandard o )
		{
			float2 uv_Normal = i.uv_texcoord * _Normal_ST.xy + _Normal_ST.zw;
			o.Normal = UnpackScaleNormal( tex2D( _Normal, uv_Normal ), _NormalScale );
			float2 uv_Color = i.uv_texcoord * _Color_ST.xy + _Color_ST.zw;
			o.Albedo = tex2D( _Color, uv_Color ).rgb;
			float x2_g1 = (0.0 + (i.uv_texcoord.y - _Emission_min) * (1.0 - 0.0) / (_Emission_max - _Emission_min));
			float y2_g1 = _Emission_hard;
			float localSigmoid2_g1 = Sigmoid2_g1( x2_g1 , y2_g1 );
			o.Emission = ( localSigmoid2_g1 * _EmissionColor ).rgb;
			o.Smoothness = 0.0;
			o.Alpha = 1;
		}

		ENDCG
	}
	Fallback "Diffuse"
	CustomEditor "ASEMaterialInspector"
}
/*ASEBEGIN
Version=18100
1360;73;922;926;1180.634;-44.40348;1.323262;True;False
Node;AmplifyShaderEditor.TextureCoordinatesNode;8;-1072,256;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;10;-1072,384;Inherit;False;Property;_Emission_min;Emission_min;4;0;Create;True;0;0;False;0;False;1;0.9;-1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;11;-1072,464;Inherit;False;Property;_Emission_max;Emission_max;5;0;Create;True;0;0;False;0;False;1;1;-1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;14;-1072,544;Inherit;False;Property;_Emission_hard;Emission_hard;6;0;Create;True;0;0;False;0;False;5;2.05;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;9;-704,272;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;15;-580.6559,551.0922;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;6;-1056,48;Inherit;False;Property;_NormalScale;NormalScale;2;0;Create;True;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;13;-432,272;Inherit;False;Sigmoid;-1;;1;190fb9d5f5d965241868590648395825;0;2;3;FLOAT;0;False;4;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;4;-448,384;Inherit;False;Property;_EmissionColor;EmissionColor;3;1;[HDR];Create;True;0;0;False;0;False;1,1,1,0;6.479555,2.876939,1.628369,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SamplerNode;1;-736,-144;Inherit;True;Property;_Color;Color;0;0;Create;True;0;0;False;0;False;-1;None;259fe6b137b908a45ba6227b5a5e5266;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SamplerNode;3;-736,48;Inherit;True;Property;_Normal;Normal;1;0;Create;True;0;0;False;0;False;-1;None;1491ec48ef7e0234998cb10c1bc806a9;True;0;True;bump;Auto;True;Object;-1;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;7;-272,112;Inherit;False;Constant;_Float0;Float 0;4;0;Create;True;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;5;-176,320;Inherit;False;2;2;0;FLOAT;0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.StandardSurfaceOutputNode;0;0,0;Float;False;True;-1;2;ASEMaterialInspector;0;0;Standard;CandleFlamesShaders/Others/Rope_01;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;Back;0;False;-1;0;False;-1;False;0;False;-1;0;False;-1;False;0;Opaque;0.5;True;True;0;False;Opaque;;Geometry;All;14;all;True;True;True;True;0;False;-1;False;0;False;-1;255;False;-1;255;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;False;2;15;10;25;False;0.5;True;0;0;False;-1;0;False;-1;0;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;0;0,0,0,0;VertexOffset;True;False;Cylindrical;False;Relative;0;;-1;-1;-1;-1;0;False;0;0;False;-1;-1;0;False;-1;0;0;0;False;0.1;False;-1;0;False;-1;16;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT;0;False;9;FLOAT;0;False;10;FLOAT;0;False;13;FLOAT3;0,0,0;False;11;FLOAT3;0,0,0;False;12;FLOAT3;0,0,0;False;14;FLOAT4;0,0,0,0;False;15;FLOAT3;0,0,0;False;0
WireConnection;9;0;8;2
WireConnection;9;1;10;0
WireConnection;9;2;11;0
WireConnection;15;0;14;0
WireConnection;13;3;9;0
WireConnection;13;4;15;0
WireConnection;3;5;6;0
WireConnection;5;0;13;0
WireConnection;5;1;4;0
WireConnection;0;0;1;0
WireConnection;0;1;3;0
WireConnection;0;2;5;0
WireConnection;0;4;7;0
ASEEND*/
//CHKSM=D9B5773D44D0C2054EFEBAEF37AF664679F4B7D8