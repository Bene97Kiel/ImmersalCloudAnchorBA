// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "CandleFlamesShaders/Fire_BorderSmooth"
{
	Properties
	{
		[HDR]_Color("Color", Color) = (1,1,1,0)
		_Fire("Fire", 2D) = "white" {}
		_FireOffset1("FireOffset", Float) = 0
		[HDR]_Color_Glow("Color_Glow", Color) = (0.6509434,0.4870858,0.3531061,0)
		_Glow("Glow", 2D) = "white" {}
		_GlowOffset1("GlowOffset", Float) = 0
		_Noise("Noise", 2D) = "gray" {}
		_NoiseSize("NoiseSize", Float) = 1
		_TimeScale("TimeScale", Float) = 1
		_NoiseFactor("NoiseFactor", Range( 0 , 1)) = 0
		_NoiseUVLock("NoiseUVLock", Range( 0 , 1)) = 0
		_Size("Size", Float) = 1
		_UVClamp("UV Clamp", Range( 0 , 1)) = 1
		_SizeFireMultiply1("SizeFireMultiply", Float) = 1
		_SmoothBorder1("SmoothBorder", Range( 0 , 1)) = 0

	}
	
	SubShader
	{
		
		
		Tags { "RenderType"="Overlay" }
	LOD 100

		CGINCLUDE
		#pragma target 3.0
		ENDCG
		Blend One One
		Cull Back
		ColorMask RGBA
		ZWrite Off
		ZTest LEqual
		
		
		
		Pass
		{
			Name "Unlit"
			Tags { "LightMode"="ForwardBase" }
			CGPROGRAM

			#define ASE_ABSOLUTE_VERTEX_POS 1


			#ifndef UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX
			//only defining to not throw compilation error over Unity 5.5
			#define UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(input)
			#endif
			#pragma vertex vert
			#pragma fragment frag
			#pragma multi_compile_instancing
			#include "UnityCG.cginc"
			#include "UnityShaderVariables.cginc"
			#define ASE_NEEDS_VERT_POSITION


			struct appdata
			{
				float4 vertex : POSITION;
				float4 color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				float4 ase_texcoord : TEXCOORD0;
			};
			
			struct v2f
			{
				float4 vertex : SV_POSITION;
#ifdef ASE_NEEDS_FRAG_WORLD_POSITION
				float3 worldPos : TEXCOORD0;
#endif
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
				float4 ase_texcoord1 : TEXCOORD1;
				float4 ase_texcoord2 : TEXCOORD2;
			};

			uniform float4 _Color_Glow;
			uniform sampler2D _Glow;
			uniform float _GlowOffset1;
			uniform float4 _Color;
			uniform sampler2D _Fire;
			uniform float _Size;
			uniform float _SizeFireMultiply1;
			uniform float _FireOffset1;
			uniform float _UVClamp;
			uniform sampler2D _Noise;
			uniform float _NoiseSize;
			uniform float _TimeScale;
			uniform float _NoiseFactor;
			uniform float _NoiseUVLock;
			UNITY_DECLARE_DEPTH_TEXTURE( _CameraDepthTexture );
			uniform float4 _CameraDepthTexture_TexelSize;
			uniform float _SmoothBorder1;

			
			v2f vert ( appdata v )
			{
				v2f o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);
				UNITY_TRANSFER_INSTANCE_ID(v, o);

				float3 vertexPos2_g5 = v.vertex.xyz;
				float4 ase_clipPos2_g5 = UnityObjectToClipPos(vertexPos2_g5);
				float4 screenPos2_g5 = ComputeScreenPos(ase_clipPos2_g5);
				o.ase_texcoord2 = screenPos2_g5;
				
				o.ase_texcoord1.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord1.zw = 0;
				float3 vertexValue = float3(0, 0, 0);
				#if ASE_ABSOLUTE_VERTEX_POS
				vertexValue = v.vertex.xyz;
				#endif
				vertexValue = vertexValue;
				#if ASE_ABSOLUTE_VERTEX_POS
				v.vertex.xyz = vertexValue;
				#else
				v.vertex.xyz += vertexValue;
				#endif
				o.vertex = UnityObjectToClipPos(v.vertex);

#ifdef ASE_NEEDS_FRAG_WORLD_POSITION
				o.worldPos = mul(unity_ObjectToWorld, v.vertex).xyz;
#endif
				return o;
			}
			
			fixed4 frag (v2f i ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID(i);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(i);
				fixed4 finalColor;
#ifdef ASE_NEEDS_FRAG_WORLD_POSITION
				float3 WorldPosition = i.worldPos;
#endif
				float3 appendResult76 = (float3(_Color_Glow.rgb));
				float3 ColorGlow286 = appendResult76;
				float2 appendResult2_g4 = (float2(0.0 , _GlowOffset1));
				float2 uv072 = i.ase_texcoord1.xy * float2( 1,1 ) + appendResult2_g4;
				float2 clampResult345 = clamp( uv072 , float2( 0,0 ) , float2( 1,1 ) );
				float3 GLOW_OUT289 = ( ColorGlow286 * tex2D( _Glow, clampResult345 ).r );
				float3 appendResult79 = (float3(_Color.rgb));
				float3 Color292 = appendResult79;
				float Size83 = _Size;
				float SizeFireMul347 = _SizeFireMultiply1;
				float temp_output_350_0 = ( Size83 * SizeFireMul347 );
				float2 temp_cast_2 = (( temp_output_350_0 + 1.0 )).xx;
				float2 appendResult2_g3 = (float2(0.0 , ( temp_output_350_0 * _FireOffset1 )));
				float2 uv09 = i.ase_texcoord1.xy * temp_cast_2 + appendResult2_g3;
				float2 temp_cast_3 = (( temp_output_350_0 / 2.0 )).xx;
				float UV_Clamp89 = _UVClamp;
				float2 appendResult82 = (float2(1.0 , UV_Clamp89));
				float2 clampResult66 = clamp( ( uv09 - temp_cast_3 ) , float2( 0,0 ) , appendResult82 );
				float NoiseSize114 = ( _NoiseSize * _Size );
				float2 uv010 = i.ase_texcoord1.xy * float2( 1,1 ) + float2( 0,0 );
				float TimeScale117 = ( _TimeScale / _Size );
				float mulTime18 = _Time.y * TimeScale117;
				float2 appendResult33 = (float2(( uv010.x + ( mulTime18 * 0.1 ) ) , ( uv010.y - mulTime18 )));
				float3 objToWorld239 = mul( unity_ObjectToWorld, float4( float3( 0,0,0 ), 1 ) ).xyz;
				float2 appendResult237 = (float2(objToWorld239.x , objToWorld239.z));
				float2 appendResult97 = (float2(tex2D( _Noise, ( NoiseSize114 * ( appendResult33 + appendResult237 ) ) ).rg));
				float NoiseFactor122 = _NoiseFactor;
				float2 UVFlame138 = clampResult66;
				float NoiseUVLock125 = _NoiseUVLock;
				float clampResult35 = clamp( ( UVFlame138.y - NoiseUVLock125 ) , 0.0 , 1.0 );
				float2 NOISE_OUT146 = ( (float2( -1,-1 ) + (appendResult97 - float2( 0,0 )) * (float2( 1,1 ) - float2( -1,-1 )) / (float2( 1,1 ) - float2( 0,0 ))) * NoiseFactor122 * clampResult35 );
				float2 clampResult93 = clamp( ( clampResult66 + NOISE_OUT146 ) , float2( 0,0 ) , float2( 1,1 ) );
				float3 appendResult7 = (float3(tex2D( _Fire, clampResult93 ).rgb));
				float3 FIRE_OUT295 = ( Color292 * appendResult7 );
				float4 screenPos2_g5 = i.ase_texcoord2;
				float4 ase_screenPosNorm2 = screenPos2_g5 / screenPos2_g5.w;
				ase_screenPosNorm2.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm2.z : ase_screenPosNorm2.z * 0.5 + 0.5;
				float screenDepth2_g5 = LinearEyeDepth(SAMPLE_DEPTH_TEXTURE( _CameraDepthTexture, ase_screenPosNorm2.xy ));
				float distanceDepth2_g5 = abs( ( screenDepth2_g5 - LinearEyeDepth( ase_screenPosNorm2.z ) ) / ( _SmoothBorder1 ) );
				float clampResult3_g5 = clamp( distanceDepth2_g5 , 0.0 , 1.0 );
				float SMOOTH_BOREDER_OUT339 = clampResult3_g5;
				
				
				finalColor = float4( ( ( GLOW_OUT289 + FIRE_OUT295 ) * SMOOTH_BOREDER_OUT339 ) , 0.0 );
				return finalColor;
			}
			ENDCG
		}
	}
	CustomEditor "ASEMaterialInspector"
	
	
}
/*ASEBEGIN
Version=18100
1360;73;922;926;4274.727;127.8792;1.249457;True;False
Node;AmplifyShaderEditor.CommentaryNode;278;-4048,512;Inherit;False;1160.133;1136.582;;25;79;77;6;120;71;286;76;74;122;121;125;124;114;134;113;89;88;117;83;131;116;54;292;346;347;INPUT_DATA;0,0.9917374,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;54;-3520,1088;Inherit;False;Property;_Size;Size;13;0;Create;True;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;116;-3472,768;Inherit;False;Property;_TimeScale;TimeScale;10;0;Create;True;0;0;False;0;False;1;1.5;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;346;-3542.388,1186.885;Inherit;False;Property;_SizeFireMultiply1;SizeFireMultiply;15;0;Create;True;0;0;False;0;False;1;1.9;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;131;-3296,768;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;83;-3136,1088;Inherit;False;Size;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;347;-3158.388,1202.885;Inherit;False;SizeFireMul;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;297;-4149.206,-48;Inherit;False;2183.206;435.5011;;20;295;78;7;293;29;93;91;147;138;66;68;82;9;90;69;343;70;350;348;349;FIRE;1,0.1554603,0,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;348;-4128,176;Inherit;False;347;SizeFireMul;1;0;OBJECT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;117;-3152,768;Inherit;False;TimeScale;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;349;-4120.503,82.49892;Inherit;False;83;Size;1;0;OBJECT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;291;-4049.241,-777.5436;Inherit;False;2514;625.5358;;23;118;18;10;145;24;144;239;237;33;238;115;96;139;119;126;141;38;97;99;35;123;22;146;NOUSE;1,1,1,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;118;-3999.241,-439.5437;Inherit;False;117;TimeScale;1;0;OBJECT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;350;-3952,80;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;88;-3440,976;Inherit;False;Property;_UVClamp;UV Clamp;14;0;Create;True;0;0;False;0;False;1;0.894;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;89;-3152,976;Inherit;False;UV_Clamp;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;70;-3792,48;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;343;-3840,208;Inherit;False;FireOffset;2;;3;628f35d9329ca9d4c947a28c2859c3d0;0;1;4;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleTimeNode;18;-3775.241,-439.5437;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;145;-3535.587,-539.5931;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;10;-3647.587,-667.5931;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;90;-3600,272;Inherit;False;89;UV_Clamp;1;0;OBJECT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;113;-3472,864;Inherit;False;Property;_NoiseSize;NoiseSize;9;0;Create;True;0;0;False;0;False;1;0.35;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;9;-3664,48;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;2,2;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleDivideOpNode;69;-3552,176;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.TransformPositionNode;239;-3654.249,-340.0079;Inherit;False;Object;World;False;Fast;True;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.DynamicAppendNode;82;-3408,192;Inherit;False;FLOAT2;4;0;FLOAT;1;False;1;FLOAT;1;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;24;-3375.587,-459.5931;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;144;-3356.547,-644.7713;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;68;-3408,64;Inherit;False;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;134;-3296,864;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;237;-3350.587,-291.593;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;114;-3152,864;Inherit;False;NoiseSize;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;66;-3264,64;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT2;1,1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.DynamicAppendNode;33;-3231.587,-635.5932;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;138;-3072,64;Inherit;False;UVFlame;-1;True;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;115;-3119.241,-727.5436;Inherit;False;114;NoiseSize;1;0;OBJECT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;238;-3071.241,-647.5438;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;124;-3552,560;Inherit;False;Property;_NoiseUVLock;NoiseUVLock;12;0;Create;True;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;96;-2895.241,-711.5436;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;139;-2751.241,-487.5437;Inherit;False;138;UVFlame;1;0;OBJECT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;125;-3168,560;Inherit;False;NoiseUVLock;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;119;-2751.241,-711.5436;Inherit;True;Property;_TextureSample2;Texture Sample 2;8;0;Create;True;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Instance;120;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;126;-2463.241,-391.5437;Inherit;False;125;NoiseUVLock;1;0;OBJECT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.BreakToComponentsNode;141;-2559.241,-487.5437;Inherit;False;FLOAT2;1;0;FLOAT2;0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.RangedFloatNode;121;-3568,656;Inherit;False;Property;_NoiseFactor;NoiseFactor;11;0;Create;True;0;0;False;0;False;0;0.21;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;97;-2447.241,-711.5436;Inherit;False;FLOAT2;4;0;FLOAT2;0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;38;-2239.241,-455.5437;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;122;-3168,656;Inherit;False;NoiseFactor;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;123;-2319.241,-535.5437;Inherit;False;122;NoiseFactor;1;0;OBJECT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;99;-2292.971,-716.6788;Inherit;False;5;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT2;1,1;False;3;FLOAT2;-1,-1;False;4;FLOAT2;1,1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.ClampOpNode;35;-2079.241,-455.5437;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;22;-1919.241,-535.5437;Inherit;False;3;3;0;FLOAT2;0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;146;-1759.241,-535.5437;Inherit;False;NOISE_OUT;-1;True;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.CommentaryNode;287;-4177.057,-1280;Inherit;False;1317.593;378.298;;7;289;75;73;285;72;344;345;GLOW;1,0.7722037,0,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;147;-3248,192;Inherit;False;146;NOISE_OUT;1;0;OBJECT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.FunctionNode;344;-4160,-1136;Inherit;False;GlowOffset;6;;4;f680843abbc9e1d42975fa0111715de2;0;0;1;FLOAT2;0
Node;AmplifyShaderEditor.ColorNode;77;-3984,1424;Inherit;False;Property;_Color;Color;0;1;[HDR];Create;True;0;0;False;0;False;1,1,1,0;1,1,1,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;74;-3984,1248;Inherit;False;Property;_Color_Glow;Color_Glow;4;1;[HDR];Create;True;0;0;False;0;False;0.6509434,0.4870858,0.3531061,0;0.5660378,0.3536974,0.2162692,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleAddOpNode;91;-3056,144;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.DynamicAppendNode;79;-3776,1424;Inherit;False;FLOAT3;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.DynamicAppendNode;76;-3776,1312;Inherit;False;FLOAT3;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ClampOpNode;93;-2928,144;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT2;1,1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;72;-3984,-1136;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;286;-3616,1312;Inherit;False;ColorGlow;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ClampOpNode;345;-3744,-1136;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT2;1,1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;292;-3600,1424;Inherit;False;Color;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SamplerNode;29;-2784,80;Inherit;True;Property;_TextureSample0;Texture Sample 0;1;0;Create;True;0;0;False;0;False;-1;None;None;True;0;False;white;LockedToTexture2D;False;Instance;6;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DynamicAppendNode;7;-2496,80;Inherit;False;FLOAT3;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;285;-3472,-1232;Inherit;False;286;ColorGlow;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SamplerNode;73;-3584,-1136;Inherit;True;Property;_TextureSample1;Texture Sample 1;5;0;Create;True;0;0;False;0;False;-1;None;None;True;0;False;white;LockedToTexture2D;False;Instance;71;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;337;-2800,1232;Inherit;False;750.0267;197.8844;Comment;3;342;339;341;SMOOTH_BORDER;0.713863,1,0,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;293;-2528,0;Inherit;False;292;Color;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;78;-2336,16;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;75;-3264,-1136;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.PosVertexDataNode;342;-2768,1280;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;289;-3120,-1136;Inherit;False;GLOW_OUT;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FunctionNode;341;-2560,1280;Inherit;False;SmoothBorder;16;;5;6e5b5da19764aa24d9e31fc6220bb2ab;0;1;4;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;295;-2192,16;Inherit;False;FIRE_OUT;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;301;-2800,720;Inherit;False;865.7219;432.8958;;6;338;234;330;80;296;290;OUT;0.9751091,0,1,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;296;-2720,848;Inherit;False;295;FIRE_OUT;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;290;-2720,768;Inherit;False;289;GLOW_OUT;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;339;-2320,1280;Inherit;False;SMOOTH_BOREDER_OUT;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;80;-2528,800;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;338;-2720,928;Inherit;False;339;SMOOTH_BOREDER_OUT;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;245;-413.9461,-1300.454;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;4;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;120;-4000,1024;Inherit;True;Property;_Noise;Noise;8;0;Create;True;0;0;False;0;False;-1;None;0ec651b261a731243b9fe269a3a5bfae;True;0;False;gray;Auto;False;Object;-1;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;330;-2368,800;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;264;-1223.874,-962.6716;Inherit;False;Constant;_Float0;Float 0;11;0;Create;True;0;0;False;0;False;0.7;0.7616114;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.ViewDirInputsCoordNode;250;-1350.784,-1401.914;Inherit;False;World;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.DotProductOpNode;259;-1087.694,-1246.724;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DotProductOpNode;253;-1058.248,-1439.248;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;260;-569.9807,-1302.079;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;261;-346.062,-1204.25;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.WorldNormalVector;249;-1373.449,-1543.248;Inherit;False;False;1;0;FLOAT3;0,0,1;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.AbsOpNode;262;-770.8384,-1108.965;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;71;-4000,816;Inherit;True;Property;_Glow;Glow;5;0;Create;True;0;0;False;0;False;-1;None;71b330055cdae7d438bb0d46ea78e928;True;0;False;white;LockedToTexture2D;False;Object;-1;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;263;-650.3734,-1109.672;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;258;-1297.246,-1160.613;Inherit;False;Constant;_Vector0;Vector 0;11;0;Create;True;0;0;False;0;False;0,1,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleSubtractOpNode;265;-949.3549,-1206.05;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.3;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;6;-4000,608;Inherit;True;Property;_Fire;Fire;1;0;Create;True;0;0;False;0;False;-1;None;8750e76ae4cdd5e41843de59565b5182;True;0;False;white;LockedToTexture2D;False;Object;-1;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;244;-176.84,-1080.664;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;234;-2176,880;Float;False;True;-1;2;ASEMaterialInspector;100;1;CandleFlamesShaders/Fire_BorderSmooth;0770190933193b94aaa3065e307002fa;True;Unlit;0;0;Unlit;2;True;4;1;False;-1;1;False;-1;0;2;False;-1;0;False;-1;True;0;False;-1;0;False;-1;True;False;True;0;False;-1;True;True;True;True;True;0;False;-1;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;True;2;False;-1;True;3;False;-1;True;False;0;False;-1;0;False;-1;True;1;RenderType=Overlay=RenderType;True;2;0;False;False;False;False;False;False;False;False;False;True;1;LightMode=ForwardBase;False;0;;0;0;Standard;1;Vertex Position,InvertActionOnDeselection;0;0;1;True;False;;0
WireConnection;131;0;116;0
WireConnection;131;1;54;0
WireConnection;83;0;54;0
WireConnection;347;0;346;0
WireConnection;117;0;131;0
WireConnection;350;0;349;0
WireConnection;350;1;348;0
WireConnection;89;0;88;0
WireConnection;70;0;350;0
WireConnection;343;4;350;0
WireConnection;18;0;118;0
WireConnection;145;0;18;0
WireConnection;9;0;70;0
WireConnection;9;1;343;0
WireConnection;69;0;350;0
WireConnection;82;1;90;0
WireConnection;24;0;10;2
WireConnection;24;1;18;0
WireConnection;144;0;10;1
WireConnection;144;1;145;0
WireConnection;68;0;9;0
WireConnection;68;1;69;0
WireConnection;134;0;113;0
WireConnection;134;1;54;0
WireConnection;237;0;239;1
WireConnection;237;1;239;3
WireConnection;114;0;134;0
WireConnection;66;0;68;0
WireConnection;66;2;82;0
WireConnection;33;0;144;0
WireConnection;33;1;24;0
WireConnection;138;0;66;0
WireConnection;238;0;33;0
WireConnection;238;1;237;0
WireConnection;96;0;115;0
WireConnection;96;1;238;0
WireConnection;125;0;124;0
WireConnection;119;1;96;0
WireConnection;141;0;139;0
WireConnection;97;0;119;0
WireConnection;38;0;141;1
WireConnection;38;1;126;0
WireConnection;122;0;121;0
WireConnection;99;0;97;0
WireConnection;35;0;38;0
WireConnection;22;0;99;0
WireConnection;22;1;123;0
WireConnection;22;2;35;0
WireConnection;146;0;22;0
WireConnection;91;0;66;0
WireConnection;91;1;147;0
WireConnection;79;0;77;0
WireConnection;76;0;74;0
WireConnection;93;0;91;0
WireConnection;72;1;344;0
WireConnection;286;0;76;0
WireConnection;345;0;72;0
WireConnection;292;0;79;0
WireConnection;29;1;93;0
WireConnection;7;0;29;0
WireConnection;73;1;345;0
WireConnection;78;0;293;0
WireConnection;78;1;7;0
WireConnection;75;0;285;0
WireConnection;75;1;73;1
WireConnection;289;0;75;0
WireConnection;341;4;342;0
WireConnection;295;0;78;0
WireConnection;339;0;341;0
WireConnection;80;0;290;0
WireConnection;80;1;296;0
WireConnection;245;0;260;0
WireConnection;330;0;80;0
WireConnection;330;1;338;0
WireConnection;259;0;250;0
WireConnection;259;1;258;0
WireConnection;253;0;249;0
WireConnection;253;1;250;0
WireConnection;260;0;253;0
WireConnection;260;1;263;0
WireConnection;261;0;245;0
WireConnection;262;0;265;0
WireConnection;263;0;262;0
WireConnection;263;1;264;0
WireConnection;265;0;259;0
WireConnection;244;0;261;0
WireConnection;234;0;330;0
ASEEND*/
//CHKSM=F8B440888F1E1D63FBF65068CFCF20F9FF4F3A38