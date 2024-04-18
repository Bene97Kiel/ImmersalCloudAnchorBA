// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "CandleFlamesShaders/Others/Candlestick_2"
{
	Properties
	{
		_Color1("Color1", Color) = (1,0,0,0)
		_Color2("Color2", Color) = (1,0,0,0)
		_GradientMin("GradientMin", Range( 0 , 1)) = 0
		_GradientMax("GradientMax", Range( 0 , 1)) = 1
		_GradientFactor("GradientFactor", Range( -1 , 1)) = 1
		_Metal("Metal", Range( 0 , 1)) = 1
		_Specular("Specular", Range( 0 , 1)) = 0
		[HideInInspector] _texcoord( "", 2D ) = "white" {}
		[HideInInspector] __dirty( "", Int ) = 1
	}

	SubShader
	{
		Tags{ "RenderType" = "Opaque"  "Queue" = "Geometry+0" }
		Cull Back
		CGPROGRAM
		#pragma target 3.0
		#pragma surface surf Standard keepalpha addshadow fullforwardshadows 
		struct Input
		{
			float2 uv_texcoord;
		};

		uniform float4 _Color1;
		uniform float4 _Color2;
		uniform float _GradientMin;
		uniform float _GradientMax;
		uniform float _GradientFactor;
		uniform float _Metal;
		uniform float _Specular;

		void surf( Input i , inout SurfaceOutputStandard o )
		{
			float clampResult7 = clamp( (0.0 + (i.uv_texcoord.y - _GradientMin) * (1.0 - 0.0) / (_GradientMax - _GradientMin)) , 0.0 , 1.0 );
			float4 lerpResult15 = lerp( _Color1 , _Color2 , ( clampResult7 * _GradientFactor ));
			o.Albedo = lerpResult15.rgb;
			o.Metallic = _Metal;
			o.Smoothness = _Specular;
			o.Alpha = 1;
		}

		ENDCG
	}
	Fallback "Diffuse"
	CustomEditor "ASEMaterialInspector"
}
/*ASEBEGIN
Version=18100
1360;73;922;926;1574.566;710.4783;1.58968;True;False
Node;AmplifyShaderEditor.TextureCoordinatesNode;4;-1328,-112;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;8;-1392,16;Inherit;False;Property;_GradientMin;GradientMin;2;0;Create;True;0;0;False;0;False;0;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;9;-1392,96;Inherit;False;Property;_GradientMax;GradientMax;3;0;Create;True;0;0;False;0;False;1;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;6;-1024,-96;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;7;-832,-96;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;12;-1392,176;Inherit;False;Property;_GradientFactor;GradientFactor;4;0;Create;True;0;0;False;0;False;1;0.87;-1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;13;-656,-48;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;14;-944,-512;Inherit;False;Property;_Color2;Color2;1;0;Create;True;0;0;False;0;False;1,0,0,0;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;1;-944,-336;Inherit;False;Property;_Color1;Color1;0;0;Create;True;0;0;False;0;False;1,0,0,0;1,0.4669811,0.4669811,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;2;-512,96;Inherit;False;Property;_Metal;Metal;5;0;Create;True;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;3;-512,176;Inherit;False;Property;_Specular;Specular;6;0;Create;True;0;0;False;0;False;0;0.74;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;15;-429.6864,-407.5335;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.StandardSurfaceOutputNode;0;0,0;Float;False;True;-1;2;ASEMaterialInspector;0;0;Standard;CandleFlamesShaders/Others/Candlestick_2;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;Back;0;False;-1;0;False;-1;False;0;False;-1;0;False;-1;False;0;Opaque;0.5;True;True;0;False;Opaque;;Geometry;All;14;all;True;True;True;True;0;False;-1;False;0;False;-1;255;False;-1;255;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;False;2;15;10;25;False;0.5;True;0;0;False;-1;0;False;-1;0;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;0;0,0,0,0;VertexOffset;True;False;Cylindrical;False;Relative;0;;-1;-1;-1;-1;0;False;0;0;False;-1;-1;0;False;-1;0;0;0;False;0.1;False;-1;0;False;-1;16;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT;0;False;9;FLOAT;0;False;10;FLOAT;0;False;13;FLOAT3;0,0,0;False;11;FLOAT3;0,0,0;False;12;FLOAT3;0,0,0;False;14;FLOAT4;0,0,0,0;False;15;FLOAT3;0,0,0;False;0
WireConnection;6;0;4;2
WireConnection;6;1;8;0
WireConnection;6;2;9;0
WireConnection;7;0;6;0
WireConnection;13;0;7;0
WireConnection;13;1;12;0
WireConnection;15;0;1;0
WireConnection;15;1;14;0
WireConnection;15;2;13;0
WireConnection;0;0;15;0
WireConnection;0;3;2;0
WireConnection;0;4;3;0
ASEEND*/
//CHKSM=8869CC7E23727D6577A928D9EEF63118BC040BB5