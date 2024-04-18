// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "CandleFlamesShaders/Others/Table"
{
	Properties
	{
		_brown_planks_03_AO_2k("brown_planks_03_AO_2k", 2D) = "white" {}
		_brown_planks_03_diff_2k("brown_planks_03_diff_2k", 2D) = "white" {}
		_brown_planks_03_Nor_2k("brown_planks_03_Nor_2k", 2D) = "bump" {}
		_brown_planks_03_rough_2k("brown_planks_03_rough_2k", 2D) = "white" {}
		_Pow("Pow", Float) = 1
		_Mull("Mull", Float) = 1
		_RoughnessMull("RoughnessMull", Range( 0 , 1)) = 0.3058824
		[HideInInspector] _texcoord( "", 2D ) = "white" {}
		[HideInInspector] __dirty( "", Int ) = 1
	}

	SubShader
	{
		Tags{ "RenderType" = "Opaque"  "Queue" = "Geometry+0" }
		Cull Back
		CGINCLUDE
		#include "UnityPBSLighting.cginc"
		#include "Lighting.cginc"
		#pragma target 3.0
		#ifdef UNITY_PASS_SHADOWCASTER
			#undef INTERNAL_DATA
			#undef WorldReflectionVector
			#undef WorldNormalVector
			#define INTERNAL_DATA half3 internalSurfaceTtoW0; half3 internalSurfaceTtoW1; half3 internalSurfaceTtoW2;
			#define WorldReflectionVector(data,normal) reflect (data.worldRefl, half3(dot(data.internalSurfaceTtoW0,normal), dot(data.internalSurfaceTtoW1,normal), dot(data.internalSurfaceTtoW2,normal)))
			#define WorldNormalVector(data,normal) half3(dot(data.internalSurfaceTtoW0,normal), dot(data.internalSurfaceTtoW1,normal), dot(data.internalSurfaceTtoW2,normal))
		#endif
		struct Input
		{
			float3 worldNormal;
			INTERNAL_DATA
			float2 uv_texcoord;
			float3 worldPos;
		};

		struct SurfaceOutputCustomLightingCustom
		{
			half3 Albedo;
			half3 Normal;
			half3 Emission;
			half Metallic;
			half Smoothness;
			half Occlusion;
			half Alpha;
			Input SurfInput;
			UnityGIInput GIData;
		};

		uniform sampler2D _brown_planks_03_diff_2k;
		uniform float4 _brown_planks_03_diff_2k_ST;
		uniform sampler2D _brown_planks_03_Nor_2k;
		uniform float4 _brown_planks_03_Nor_2k_ST;
		uniform sampler2D _brown_planks_03_rough_2k;
		uniform float4 _brown_planks_03_rough_2k_ST;
		uniform float _RoughnessMull;
		uniform sampler2D _brown_planks_03_AO_2k;
		uniform float4 _brown_planks_03_AO_2k_ST;
		uniform float _Pow;
		uniform float _Mull;

		inline half4 LightingStandardCustomLighting( inout SurfaceOutputCustomLightingCustom s, half3 viewDir, UnityGI gi )
		{
			UnityGIInput data = s.GIData;
			Input i = s.SurfInput;
			half4 c = 0;
			SurfaceOutputStandard s12 = (SurfaceOutputStandard ) 0;
			float2 uv_brown_planks_03_diff_2k = i.uv_texcoord * _brown_planks_03_diff_2k_ST.xy + _brown_planks_03_diff_2k_ST.zw;
			s12.Albedo = tex2D( _brown_planks_03_diff_2k, uv_brown_planks_03_diff_2k ).rgb;
			float2 uv_brown_planks_03_Nor_2k = i.uv_texcoord * _brown_planks_03_Nor_2k_ST.xy + _brown_planks_03_Nor_2k_ST.zw;
			s12.Normal = WorldNormalVector( i , UnpackNormal( tex2D( _brown_planks_03_Nor_2k, uv_brown_planks_03_Nor_2k ) ) );
			s12.Emission = float3( 0,0,0 );
			s12.Metallic = 0.0;
			float2 uv_brown_planks_03_rough_2k = i.uv_texcoord * _brown_planks_03_rough_2k_ST.xy + _brown_planks_03_rough_2k_ST.zw;
			s12.Smoothness = ( 1.0 - ( tex2D( _brown_planks_03_rough_2k, uv_brown_planks_03_rough_2k ).r * _RoughnessMull ) );
			float2 uv_brown_planks_03_AO_2k = i.uv_texcoord * _brown_planks_03_AO_2k_ST.xy + _brown_planks_03_AO_2k_ST.zw;
			s12.Occlusion = tex2D( _brown_planks_03_AO_2k, uv_brown_planks_03_AO_2k ).r;

			data.light = gi.light;

			UnityGI gi12 = gi;
			#ifdef UNITY_PASS_FORWARDBASE
			Unity_GlossyEnvironmentData g12 = UnityGlossyEnvironmentSetup( s12.Smoothness, data.worldViewDir, s12.Normal, float3(0,0,0));
			gi12 = UnityGlobalIllumination( data, s12.Occlusion, s12.Normal, g12 );
			#endif

			float3 surfResult12 = LightingStandard ( s12, viewDir, gi12 ).rgb;
			surfResult12 += s12.Emission;

			#ifdef UNITY_PASS_FORWARDADD//12
			surfResult12 -= s12.Emission;
			#endif//12
			float3 ase_worldPos = i.worldPos;
			float3 objToWorld17 = mul( unity_ObjectToWorld, float4( float3( 0,0,0 ), 1 ) ).xyz;
			float clampResult7 = clamp( (1.0 + (( ase_worldPos - objToWorld17 ).z - 0.0) * (0.0 - 1.0) / (1.0 - 0.0)) , 0.0 , 1.0 );
			float3 temp_cast_1 = (_Pow).xxx;
			c.rgb = ( pow( ( surfResult12 * clampResult7 ) , temp_cast_1 ) * _Mull );
			c.a = 1;
			return c;
		}

		inline void LightingStandardCustomLighting_GI( inout SurfaceOutputCustomLightingCustom s, UnityGIInput data, inout UnityGI gi )
		{
			s.GIData = data;
		}

		void surf( Input i , inout SurfaceOutputCustomLightingCustom o )
		{
			o.SurfInput = i;
			o.Normal = float3(0,0,1);
		}

		ENDCG
		CGPROGRAM
		#pragma surface surf StandardCustomLighting keepalpha fullforwardshadows 

		ENDCG
		Pass
		{
			Name "ShadowCaster"
			Tags{ "LightMode" = "ShadowCaster" }
			ZWrite On
			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			#pragma target 3.0
			#pragma multi_compile_shadowcaster
			#pragma multi_compile UNITY_PASS_SHADOWCASTER
			#pragma skip_variants FOG_LINEAR FOG_EXP FOG_EXP2
			#include "HLSLSupport.cginc"
			#if ( SHADER_API_D3D11 || SHADER_API_GLCORE || SHADER_API_GLES || SHADER_API_GLES3 || SHADER_API_METAL || SHADER_API_VULKAN )
				#define CAN_SKIP_VPOS
			#endif
			#include "UnityCG.cginc"
			#include "Lighting.cginc"
			#include "UnityPBSLighting.cginc"
			struct v2f
			{
				V2F_SHADOW_CASTER;
				float2 customPack1 : TEXCOORD1;
				float4 tSpace0 : TEXCOORD2;
				float4 tSpace1 : TEXCOORD3;
				float4 tSpace2 : TEXCOORD4;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};
			v2f vert( appdata_full v )
			{
				v2f o;
				UNITY_SETUP_INSTANCE_ID( v );
				UNITY_INITIALIZE_OUTPUT( v2f, o );
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );
				UNITY_TRANSFER_INSTANCE_ID( v, o );
				Input customInputData;
				float3 worldPos = mul( unity_ObjectToWorld, v.vertex ).xyz;
				half3 worldNormal = UnityObjectToWorldNormal( v.normal );
				half3 worldTangent = UnityObjectToWorldDir( v.tangent.xyz );
				half tangentSign = v.tangent.w * unity_WorldTransformParams.w;
				half3 worldBinormal = cross( worldNormal, worldTangent ) * tangentSign;
				o.tSpace0 = float4( worldTangent.x, worldBinormal.x, worldNormal.x, worldPos.x );
				o.tSpace1 = float4( worldTangent.y, worldBinormal.y, worldNormal.y, worldPos.y );
				o.tSpace2 = float4( worldTangent.z, worldBinormal.z, worldNormal.z, worldPos.z );
				o.customPack1.xy = customInputData.uv_texcoord;
				o.customPack1.xy = v.texcoord;
				TRANSFER_SHADOW_CASTER_NORMALOFFSET( o )
				return o;
			}
			half4 frag( v2f IN
			#if !defined( CAN_SKIP_VPOS )
			, UNITY_VPOS_TYPE vpos : VPOS
			#endif
			) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				Input surfIN;
				UNITY_INITIALIZE_OUTPUT( Input, surfIN );
				surfIN.uv_texcoord = IN.customPack1.xy;
				float3 worldPos = float3( IN.tSpace0.w, IN.tSpace1.w, IN.tSpace2.w );
				half3 worldViewDir = normalize( UnityWorldSpaceViewDir( worldPos ) );
				surfIN.worldPos = worldPos;
				surfIN.worldNormal = float3( IN.tSpace0.z, IN.tSpace1.z, IN.tSpace2.z );
				surfIN.internalSurfaceTtoW0 = IN.tSpace0.xyz;
				surfIN.internalSurfaceTtoW1 = IN.tSpace1.xyz;
				surfIN.internalSurfaceTtoW2 = IN.tSpace2.xyz;
				SurfaceOutputCustomLightingCustom o;
				UNITY_INITIALIZE_OUTPUT( SurfaceOutputCustomLightingCustom, o )
				surf( surfIN, o );
				#if defined( CAN_SKIP_VPOS )
				float2 vpos = IN.pos;
				#endif
				SHADOW_CASTER_FRAGMENT( IN )
			}
			ENDCG
		}
	}
	Fallback "Diffuse"
	CustomEditor "ASEMaterialInspector"
}
/*ASEBEGIN
Version=18100
1360;73;922;926;2103.748;467.4859;1.387066;True;False
Node;AmplifyShaderEditor.WorldPosInputsNode;6;-1440,288;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.TransformPositionNode;17;-1472,432;Inherit;False;Object;World;False;Fast;True;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleSubtractOpNode;18;-1200,288;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;29;-1904,32;Inherit;False;Property;_RoughnessMull;RoughnessMull;6;0;Create;True;0;0;False;0;False;0.3058824;0.3058824;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;4;-1920,-160;Inherit;True;Property;_brown_planks_03_rough_2k;brown_planks_03_rough_2k;3;0;Create;True;0;0;False;0;False;-1;f76f8b18900c1eb4bacd56ba8f5c41af;f76f8b18900c1eb4bacd56ba8f5c41af;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;30;-1440,-144;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.BreakToComponentsNode;21;-1056,288;Inherit;False;FLOAT3;1;0;FLOAT3;0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.SamplerNode;3;-1485.545,-347.9734;Inherit;True;Property;_brown_planks_03_Nor_2k;brown_planks_03_Nor_2k;2;0;Create;True;0;0;False;0;False;-1;1e05f42c77f96944f9ccef675eba93eb;1e05f42c77f96944f9ccef675eba93eb;True;0;True;bump;Auto;True;Object;-1;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SamplerNode;2;-1485.545,-539.9736;Inherit;True;Property;_brown_planks_03_diff_2k;brown_planks_03_diff_2k;1;0;Create;True;0;0;False;0;False;-1;a2d48a634e7e78f46afa10ee0caa55b5;a2d48a634e7e78f46afa10ee0caa55b5;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TFHCRemapNode;15;-832,288;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;1;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;13;-1280,-144;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;1;-1485.545,36.02648;Inherit;True;Property;_brown_planks_03_AO_2k;brown_planks_03_AO_2k;0;0;Create;True;0;0;False;0;False;-1;ac0840e2c2ee8494da928726c7157a53;ac0840e2c2ee8494da928726c7157a53;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ClampOpNode;7;-640,288;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.CustomStandardSurface;12;-976,-272;Inherit;False;Metallic;Tangent;6;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,1;False;2;FLOAT3;0,0,0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;1;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;19;-432,192;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;26;-432,304;Inherit;False;Property;_Pow;Pow;4;0;Create;True;0;0;False;0;False;1;1.61;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;25;-256,192;Inherit;False;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;1;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;28;-256,304;Inherit;False;Property;_Mull;Mull;5;0;Create;True;0;0;False;0;False;1;2.08;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;27;-64,224;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.StandardSurfaceOutputNode;0;244.1957,-13.79637;Float;False;True;-1;2;ASEMaterialInspector;0;0;CustomLighting;CandleFlamesShaders/Others/Table;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;Back;0;False;-1;0;False;-1;False;0;False;-1;0;False;-1;False;0;Opaque;0.5;True;True;0;False;Opaque;;Geometry;All;14;all;True;True;True;True;0;False;-1;False;0;False;-1;255;False;-1;255;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;False;2;15;10;25;False;0.5;True;0;0;False;-1;0;False;-1;0;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;0;0,0,0,0;VertexOffset;True;False;Cylindrical;False;Relative;0;;-1;-1;-1;-1;0;False;0;0;False;-1;-1;0;False;-1;0;0;0;False;0.1;False;-1;0;False;-1;15;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT;0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT;0;False;9;FLOAT;0;False;10;FLOAT;0;False;13;FLOAT3;0,0,0;False;11;FLOAT3;0,0,0;False;12;FLOAT3;0,0,0;False;14;FLOAT4;0,0,0,0;False;15;FLOAT3;0,0,0;False;0
WireConnection;18;0;6;0
WireConnection;18;1;17;0
WireConnection;30;0;4;1
WireConnection;30;1;29;0
WireConnection;21;0;18;0
WireConnection;15;0;21;2
WireConnection;13;0;30;0
WireConnection;7;0;15;0
WireConnection;12;0;2;0
WireConnection;12;1;3;0
WireConnection;12;4;13;0
WireConnection;12;5;1;1
WireConnection;19;0;12;0
WireConnection;19;1;7;0
WireConnection;25;0;19;0
WireConnection;25;1;26;0
WireConnection;27;0;25;0
WireConnection;27;1;28;0
WireConnection;0;13;27;0
ASEEND*/
//CHKSM=F3E336B0EAB23FC7FB801EA76582889F26E4510C