// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "CandleFlamesShaders/WaxQueue_Tessellation"
{
	Properties
	{
		_EdgeLength ( "Edge length", Range( 2, 50 ) ) = 14.5
		_FirstColor("FirstColor", Color) = (0.8962264,0.8962264,0.8962264,0)
		_SecondColor("SecondColor", Color) = (1,1,1,0)
		[Toggle(_EMISSIONENABLE_ON)] _EmissionEnable("EmissionEnable", Float) = 1
		[HDR]_EmissionColor_1("EmissionColor_1", Color) = (1.741101,1.741101,1.741101,0)
		[HDR]_EmissionColor_2("EmissionColor_2", Color) = (1.741101,1.741101,1.741101,0)
		_EmissionPow("EmissionPow", Range( 0 , 10)) = 0
		_EmissionFactor("EmissionFactor", Float) = 0
		_EmissionFlowFactor("EmissionFlowFactor", Range( 0 , 0.7)) = 0
		_High("High", 2D) = "black" {}
		_VertexOffset("VertexOffset", Range( 0 , 1)) = 0.05
		_SmoothnessMax("SmoothnessMax", Range( 0 , 1)) = 1
		_SmoothnessMin("SmoothnessMin", Range( 0 , 1)) = 1
		_SpecularOffset("SpecularOffset", Range( -1 , 1)) = 0
		_Normal("Normal", 2D) = "bump" {}
		_FirstScaleNormal("FirstScaleNormal", Float) = 1
		_SecondScaleNormal("SecondScaleNormal", Float) = 0.3
		_HighMax("HighMax", Range( 0 , 1)) = 0
		_TimeScale("TimeScale", Float) = 0.05
		_TimeSecondMull("TimeSecondMull", Float) = 3
		_TextureOffset("TextureOffset", Vector) = (1,1,2,2)
		_Talling("Talling", Vector) = (1,1,2,2)
		[Toggle(_EMISSIONNOISE_ON)] _EmissionNoise("EmissionNoise", Float) = 0
		_EmissionNoise1("EmissionNoise", Float) = 1
		_EmissionNoiseMin1("EmissionNoiseMin", Range( 0 , 1)) = 0.5
		_EmissionTimeScale1("EmissionTimeScale", Float) = 1
		[Toggle(_VERTEXCOLORMASK_ON)] _VertexColorMask("VertexColorMask", Float) = 0
		[HideInInspector] _texcoord( "", 2D ) = "white" {}
		[HideInInspector] __dirty( "", Int ) = 1
	}

	SubShader
	{
		Tags{ "RenderType" = "Opaque"  "Queue" = "Geometry+0" "IgnoreProjector" = "True" "IsEmissive" = "true"  }
		Cull Back
		CGPROGRAM
		#include "UnityShaderVariables.cginc"
		#include "UnityStandardUtils.cginc"
		#include "Tessellation.cginc"
		#pragma target 4.6
		#pragma shader_feature_local _VERTEXCOLORMASK_ON
		#pragma shader_feature_local _EMISSIONENABLE_ON
		#pragma shader_feature_local _EMISSIONNOISE_ON
		#pragma surface surf Standard keepalpha addshadow fullforwardshadows vertex:vertexDataFunc tessellate:tessFunction 
		struct Input
		{
			float2 uv_texcoord;
			float4 vertexColor : COLOR;
		};

		uniform sampler2D _High;
		uniform float4 _Talling;
		uniform float _TimeScale;
		uniform float4 _TextureOffset;
		uniform float _HighMax;
		uniform float _VertexOffset;
		uniform sampler2D _Normal;
		uniform float _FirstScaleNormal;
		uniform float _SecondScaleNormal;
		uniform float _TimeSecondMull;
		uniform float4 _FirstColor;
		uniform float4 _SecondColor;
		uniform float4 _EmissionColor_2;
		uniform float4 _EmissionColor_1;
		uniform float _EmissionFlowFactor;
		uniform float _EmissionPow;
		uniform float _EmissionFactor;
		uniform float _EmissionTimeScale1;
		uniform float _EmissionNoise1;
		uniform float _EmissionNoiseMin1;
		uniform float _SmoothnessMin;
		uniform float _SmoothnessMax;
		uniform float _SpecularOffset;
		uniform float _EdgeLength;


		float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }

		float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }

		float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }

		float snoise( float2 v )
		{
			const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
			float2 i = floor( v + dot( v, C.yy ) );
			float2 x0 = v - i + dot( i, C.xx );
			float2 i1;
			i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
			float4 x12 = x0.xyxy + C.xxzz;
			x12.xy -= i1;
			i = mod2D289( i );
			float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
			float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
			m = m * m;
			m = m * m;
			float3 x = 2.0 * frac( p * C.www ) - 1.0;
			float3 h = abs( x ) - 0.5;
			float3 ox = floor( x + 0.5 );
			float3 a0 = x - ox;
			m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
			float3 g;
			g.x = a0.x * x0.x + h.x * x0.y;
			g.yz = a0.yz * x12.xz + h.yz * x12.yw;
			return 130.0 * dot( m, g );
		}


		float4 tessFunction( appdata_full v0, appdata_full v1, appdata_full v2 )
		{
			return UnityEdgeLengthBasedTess (v0.vertex, v1.vertex, v2.vertex, _EdgeLength);
		}

		void vertexDataFunc( inout appdata_full v )
		{
			float clampResult44 = clamp( (0.0 + (v.texcoord.xy.y - 0.8) * (1.0 - 0.0) / (0.95 - 0.8)) , 0.0 , 1.0 );
			#ifdef _VERTEXCOLORMASK_ON
				float staticSwitch264 = v.color.r;
			#else
				float staticSwitch264 = 1.0;
			#endif
			float _OUT_FLOW_MASK_181 = max( ( ( 1.0 - clampResult44 ) * v.texcoord.xy.y * staticSwitch264 ) , 0.0 );
			float2 appendResult163 = (float2(_Talling.x , _Talling.y));
			float2 FirstTiling165 = appendResult163;
			float TimeScale148 = _TimeScale;
			float mulTime4 = _Time.y * TimeScale148;
			float _OUT_TIME_154 = mulTime4;
			float2 appendResult5 = (float2(0.0 , _OUT_TIME_154));
			float2 appendResult187 = (float2(_TextureOffset.x , _TextureOffset.y));
			float2 FirstTexOffset189 = appendResult187;
			float2 uv_TexCoord3 = v.texcoord.xy * FirstTiling165 + ( appendResult5 + FirstTexOffset189 );
			float HighMax198 = _HighMax;
			float _OUT_HIGH_203 = max( (0.0 + (( _OUT_FLOW_MASK_181 * tex2Dlod( _High, float4( uv_TexCoord3, 0, 0.0) ).r ) - 0.0) * (1.0 - 0.0) / (HighMax198 - 0.0)) , 0.0 );
			float3 ase_vertexNormal = v.normal.xyz;
			float VertexOffset204 = _VertexOffset;
			float3 _OUT_VERTEx_OFFSET_210 = ( _OUT_HIGH_203 * ase_vertexNormal * VertexOffset204 );
			v.vertex.xyz += _OUT_VERTEx_OFFSET_210;
		}

		void surf( Input i , inout SurfaceOutputStandard o )
		{
			float clampResult44 = clamp( (0.0 + (i.uv_texcoord.y - 0.8) * (1.0 - 0.0) / (0.95 - 0.8)) , 0.0 , 1.0 );
			#ifdef _VERTEXCOLORMASK_ON
				float staticSwitch264 = i.vertexColor.r;
			#else
				float staticSwitch264 = 1.0;
			#endif
			float _OUT_FLOW_MASK_181 = max( ( ( 1.0 - clampResult44 ) * i.uv_texcoord.y * staticSwitch264 ) , 0.0 );
			float FirstScaleNormal174 = _FirstScaleNormal;
			float2 appendResult163 = (float2(_Talling.x , _Talling.y));
			float2 FirstTiling165 = appendResult163;
			float TimeScale148 = _TimeScale;
			float mulTime4 = _Time.y * TimeScale148;
			float _OUT_TIME_154 = mulTime4;
			float2 appendResult5 = (float2(0.0 , _OUT_TIME_154));
			float2 appendResult187 = (float2(_TextureOffset.x , _TextureOffset.y));
			float2 FirstTexOffset189 = appendResult187;
			float2 uv_TexCoord3 = i.uv_texcoord * FirstTiling165 + ( appendResult5 + FirstTexOffset189 );
			float SecondScaleNormal175 = _SecondScaleNormal;
			float2 appendResult164 = (float2(_Talling.z , _Talling.w));
			float2 SecondTiling166 = appendResult164;
			float TimeSecondMull150 = _TimeSecondMull;
			float mulTime78 = _Time.y * ( TimeScale148 * TimeSecondMull150 );
			float _OUT_SECOND_TIME_155 = mulTime78;
			float2 appendResult77 = (float2(0.0 , _OUT_SECOND_TIME_155));
			float2 appendResult186 = (float2(_TextureOffset.z , _TextureOffset.w));
			float2 SecondTexOffset188 = appendResult186;
			float2 uv_TexCoord76 = i.uv_texcoord * SecondTiling166 + ( appendResult77 + SecondTexOffset188 );
			float3 lerpResult22 = lerp( float3(0,0,0.5) , BlendNormals( UnpackScaleNormal( tex2D( _Normal, uv_TexCoord3 ), ( _OUT_FLOW_MASK_181 * FirstScaleNormal174 ) ) , UnpackScaleNormal( tex2D( _Normal, uv_TexCoord76 ), ( _OUT_FLOW_MASK_181 * SecondScaleNormal175 ) ) ) , _OUT_FLOW_MASK_181);
			float3 _OUT_NORMAL_201 = lerpResult22;
			o.Normal = _OUT_NORMAL_201;
			float3 appendResult129 = (float3(_FirstColor.rgb));
			float3 FirstColor127 = appendResult129;
			float3 appendResult130 = (float3(_SecondColor.rgb));
			float3 SecondColor128 = appendResult130;
			float HighMax198 = _HighMax;
			float _OUT_HIGH_203 = max( (0.0 + (( _OUT_FLOW_MASK_181 * tex2D( _High, uv_TexCoord3 ).r ) - 0.0) * (1.0 - 0.0) / (HighMax198 - 0.0)) , 0.0 );
			float3 lerpResult69 = lerp( FirstColor127 , SecondColor128 , _OUT_HIGH_203);
			float3 _OUT_COLOR_224 = lerpResult69;
			o.Albedo = _OUT_COLOR_224;
			float3 appendResult246 = (float3(_EmissionColor_2.rgb));
			float3 EmissionColor_2249 = appendResult246;
			float3 appendResult247 = (float3(_EmissionColor_1.rgb));
			float3 EmissionColor_1250 = appendResult247;
			float clampResult245 = clamp( i.uv_texcoord.y , 0.01 , 0.99 );
			float EmissionFlowFactor232 = _EmissionFlowFactor;
			float EmissionPow234 = _EmissionPow;
			float temp_output_263_0 = min( pow( ( ( clampResult245 * 0.3 ) + pow( ( clampResult245 - ( _OUT_HIGH_203 * EmissionFlowFactor232 ) ) , 3.0 ) ) , EmissionPow234 ) , 1.0 );
			float3 lerpResult65 = lerp( EmissionColor_2249 , EmissionColor_1250 , temp_output_263_0);
			float EmissionFactor237 = _EmissionFactor;
			float3 objToWorld4_g2 = mul( unity_ObjectToWorld, float4( float3( 0,0,0 ), 1 ) ).xyz;
			float2 appendResult7_g2 = (float2(( objToWorld4_g2.x + objToWorld4_g2.y ) , objToWorld4_g2.z));
			float mulTime8_g2 = _Time.y * _EmissionTimeScale1;
			float simplePerlin2D9_g2 = snoise( ( appendResult7_g2 + mulTime8_g2 )*_EmissionNoise1 );
			simplePerlin2D9_g2 = simplePerlin2D9_g2*0.5 + 0.5;
			float clampResult11_g2 = clamp( (_EmissionNoiseMin1 + (simplePerlin2D9_g2 - 0.0) * (1.0 - _EmissionNoiseMin1) / (1.0 - 0.0)) , 0.0 , 1.0 );
			#ifdef _EMISSIONNOISE_ON
				float staticSwitch292 = ( EmissionFactor237 * clampResult11_g2 );
			#else
				float staticSwitch292 = EmissionFactor237;
			#endif
			float3 _OUT_EMISSION_256 = ( ( lerpResult65 * temp_output_263_0 ) * staticSwitch292 );
			#ifdef _EMISSIONENABLE_ON
				float3 staticSwitch293 = _OUT_EMISSION_256;
			#else
				float3 staticSwitch293 = float3(0,0,0);
			#endif
			o.Emission = staticSwitch293;
			float SmoothnessMin137 = _SmoothnessMin;
			float SmoothnessMax138 = _SmoothnessMax;
			float lerpResult48 = lerp( SmoothnessMin137 , SmoothnessMax138 , _OUT_HIGH_203);
			float _OUT_UP_MASK_215 = clampResult44;
			float SpecularOffset295 = _SpecularOffset;
			float _OUT_SMOOTHNESS_220 = min( ( lerpResult48 + _OUT_UP_MASK_215 + SpecularOffset295 ) , 1.0 );
			o.Smoothness = _OUT_SMOOTHNESS_220;
			o.Alpha = 1;
		}

		ENDCG
	}
	Fallback "CandleFlamesShaders/WaxQueue"
	CustomEditor "ASEMaterialInspector"
}
/*ASEBEGIN
Version=18100
1270;73;552;926;1639.255;-4736.749;1;True;False
Node;AmplifyShaderEditor.CommentaryNode;114;-3074.605,2051.908;Inherit;False;1111.492;2101;Input;48;159;158;237;236;204;128;127;129;205;130;138;123;124;137;234;228;134;166;175;174;133;188;164;173;172;186;232;231;198;150;149;196;165;189;163;187;162;185;148;142;64;60;247;246;249;250;295;296;Input;0.07629752,1,0,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;142;-2930.605,2739.908;Inherit;False;Property;_TimeScale;TimeScale;22;0;Create;True;0;0;False;0;False;0.05;0.05;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;146;-1872,2528;Inherit;False;925;253;Comment;7;78;79;151;4;147;154;155;TIME;1,0.4764151,0.4764151,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;148;-2674.605,2739.908;Inherit;False;TimeScale;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;255;-1872,3840;Inherit;False;1606.221;613.979;Comment;11;265;264;215;181;47;45;44;43;25;266;267;MASK_FLOW;0.9125825,1,0,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;147;-1824,2576;Inherit;False;148;TimeScale;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;25;-1824,4080;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleTimeNode;4;-1552,2576;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector4Node;185;-2690.605,3203.908;Inherit;False;Property;_TextureOffset;TextureOffset;24;0;Create;True;0;0;False;0;False;1,1,2,2;0,0,0,0;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TFHCRemapNode;43;-1584,3904;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0.8;False;2;FLOAT;0.95;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;44;-1392,3904;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;266;-1597.286,4198.609;Inherit;False;Constant;_VertexColorMask;VertexColorMask;23;0;Create;True;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector4Node;162;-2690.605,2963.908;Inherit;False;Property;_Talling;Talling;25;0;Create;True;0;0;False;0;False;1,1,2,2;1,1,2,2;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.VertexColorNode;265;-1568.166,4274.924;Inherit;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;154;-1376,2576;Inherit;False;_OUT_TIME_;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;254;-1872,4544;Inherit;False;3071.152;829.4456;Comment;32;201;22;81;23;184;80;19;28;82;76;183;193;176;177;168;77;192;182;157;203;259;50;11;199;2;195;3;167;194;5;191;156;TEXTURE;0.3066038,0.3536128,1,1;0;0
Node;AmplifyShaderEditor.DynamicAppendNode;187;-2498.605,3187.908;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.StaticSwitch;264;-1312,4272;Inherit;False;Property;_VertexColorMask;VertexColorMask;31;0;Create;True;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;45;-1232,3984;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;163;-2498.605,2963.908;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;189;-2338.605,3203.908;Inherit;False;FirstTexOffset;-1;True;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;156;-1204,4865;Inherit;False;154;_OUT_TIME_;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;165;-2338.605,2979.908;Inherit;False;FirstTiling;-1;True;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.DynamicAppendNode;5;-1008,4848;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;191;-1072,4944;Inherit;False;189;FirstTexOffset;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;47;-1040,4096;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;194;-832,4848;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMaxOpNode;267;-908.6572,4092.896;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;167;-880,4768;Inherit;False;165;FirstTiling;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;181;-752,4096;Inherit;False;_OUT_FLOW_MASK_;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;196;-2466.605,2819.908;Inherit;False;Property;_HighMax;HighMax;21;0;Create;True;0;0;False;0;False;0;0.6;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;3;-688,4784;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;198;-2193.605,2819.908;Inherit;False;HighMax;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;195;160,4608;Inherit;False;181;_OUT_FLOW_MASK_;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;2;96,4688;Inherit;True;Property;_WaxHigh1;WaxHigh1;13;0;Create;True;0;0;False;0;False;-1;64919ecd5330b71478486fd138668904;64919ecd5330b71478486fd138668904;True;0;False;white;Auto;False;Instance;158;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;199;432,4752;Inherit;False;198;HighMax;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;11;432,4656;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;50;608,4656;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0.6;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;149;-2930.605,2819.908;Inherit;False;Property;_TimeSecondMull;TimeSecondMull;23;0;Create;True;0;0;False;0;False;3;0.3;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;231;-2512,3440;Inherit;False;Property;_EmissionFlowFactor;EmissionFlowFactor;12;0;Create;True;0;0;False;0;False;0;0.3114735;0;0.7;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMaxOpNode;259;800,4656;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;258;-1872,5520;Inherit;False;2205.385;724.2078;Comment;22;256;67;66;292;280;65;253;263;252;291;238;62;57;235;243;58;54;245;55;52;233;227;EMISSION;1,0.2939666,0,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;232;-2208,3440;Inherit;False;EmissionFlowFactor;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;150;-2674.605,2819.908;Inherit;False;TimeSecondMull;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;203;928,4656;Inherit;False;_OUT_HIGH_;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;151;-1824,2688;Inherit;False;150;TimeSecondMull;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;227;-1792,5840;Inherit;False;203;_OUT_HIGH_;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;52;-1744,5696;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;233;-1824,5920;Inherit;False;232;EmissionFlowFactor;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;245;-1472,5728;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0.01;False;2;FLOAT;0.99;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;79;-1552,2672;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;3;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;55;-1568,5856;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.3;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;228;-2512,3536;Inherit;False;Property;_EmissionPow;EmissionPow;10;0;Create;True;0;0;False;0;False;0;1.889821;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;78;-1392,2688;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;54;-1328,5856;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;234;-2208,3536;Inherit;False;EmissionPow;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;186;-2498.605,3299.908;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.PowerNode;243;-1168,5856;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;3;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;60;-3024,3776;Inherit;False;Property;_EmissionColor_2;EmissionColor_2;9;1;[HDR];Create;True;0;0;False;0;False;1.741101,1.741101,1.741101,0;2.996078,0.3137255,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;58;-1200,5728;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.3;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;64;-3024,3968;Inherit;False;Property;_EmissionColor_1;EmissionColor_1;8;1;[HDR];Create;True;0;0;False;0;False;1.741101,1.741101,1.741101,0;1.498039,1.07451,0.5019608,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;155;-1216,2688;Inherit;False;_OUT_SECOND_TIME_;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;235;-1200,5968;Inherit;False;234;EmissionPow;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;246;-2816,3792;Inherit;False;FLOAT3;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;236;-2512,3648;Inherit;False;Property;_EmissionFactor;EmissionFactor;11;0;Create;True;0;0;False;0;False;0;0.6;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;172;-3024,3440;Inherit;False;Property;_FirstScaleNormal;FirstScaleNormal;19;0;Create;True;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;157;-1280,5152;Inherit;False;155;_OUT_SECOND_TIME_;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;247;-2816,4000;Inherit;False;FLOAT3;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.DynamicAppendNode;164;-2498.605,3075.908;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;173;-3024,3536;Inherit;False;Property;_SecondScaleNormal;SecondScaleNormal;20;0;Create;True;0;0;False;0;False;0.3;0.3;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;57;-1024,5744;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;188;-2338.605,3283.908;Inherit;False;SecondTexOffset;-1;True;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;237;-2208,3648;Inherit;False;EmissionFactor;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;182;-688,4688;Inherit;False;181;_OUT_FLOW_MASK_;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;134;-3042.605,2659.908;Inherit;False;Property;_SmoothnessMax;SmoothnessMax;15;0;Create;True;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;133;-3042.605,2579.908;Inherit;False;Property;_SmoothnessMin;SmoothnessMin;16;0;Create;True;0;0;False;0;False;1;0.15;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;174;-2784,3440;Inherit;False;FirstScaleNormal;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;166;-2338.605,3059.908;Inherit;False;SecondTiling;-1;True;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.DynamicAppendNode;77;-1008,5136;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;175;-2784,3536;Inherit;False;SecondScaleNormal;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;62;-880,5744;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;250;-2656,4000;Inherit;False;EmissionColor_1;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;249;-2656,3792;Inherit;False;EmissionColor_2;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;192;-1088,5232;Inherit;False;188;SecondTexOffset;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;253;-800,5648;Inherit;False;250;EmissionColor_1;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;238;-752,6000;Inherit;False;237;EmissionFactor;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;193;-848,5136;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.ColorNode;124;-3042.605,2371.908;Inherit;False;Property;_SecondColor;SecondColor;6;0;Create;True;0;0;False;0;False;1,1,1,0;1,1,1,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;218;-1870.668,3408;Inherit;False;1016.668;378.3066;Comment;9;216;220;214;75;48;217;140;141;297;SMOOTHNESS;1,1,1,1;0;0
Node;AmplifyShaderEditor.ColorNode;123;-3042.605,2195.908;Inherit;False;Property;_FirstColor;FirstColor;5;0;Create;True;0;0;False;0;False;0.8962264,0.8962264,0.8962264,0;0.8962264,0.8962264,0.8962264,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMinOpNode;263;-720,5744;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;177;-688,4992;Inherit;False;175;SecondScaleNormal;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;137;-2674.605,2579.908;Inherit;False;SmoothnessMin;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;176;-688,4912;Inherit;False;174;FirstScaleNormal;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;252;-800,5568;Inherit;False;249;EmissionColor_2;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FunctionNode;291;-752,6096;Inherit;False;EmissionNoise;27;;2;abbb936cbb97c0546abac87e8aebd50f;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;296;-2448,3792;Inherit;False;Property;_SpecularOffset;SpecularOffset;17;0;Create;True;0;0;False;0;False;0;0;-1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;183;-224,4768;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;168;-912,5056;Inherit;False;166;SecondTiling;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;138;-2674.605,2659.908;Inherit;False;SmoothnessMax;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;280;-496,6080;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;140;-1808,3456;Inherit;False;137;SmoothnessMin;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;141;-1808,3536;Inherit;False;138;SmoothnessMax;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;295;-2160,3792;Inherit;False;SpecularOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;217;-1808,3616;Inherit;False;203;_OUT_HIGH_;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;65;-528,5648;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;215;-983.1923,3906.889;Inherit;False;_OUT_UP_MASK_;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;129;-2834.605,2227.908;Inherit;False;FLOAT3;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;28;-352,4928;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;205;-3024,3648;Inherit;False;Property;_VertexOffset;VertexOffset;14;0;Create;True;0;0;False;0;False;0.05;0.0306;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;130;-2834.605,2403.908;Inherit;False;FLOAT3;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;82;-128,5024;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.4;False;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;76;-688,5088;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;2,2;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;216;-1808,3696;Inherit;False;215;_OUT_UP_MASK_;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;225;-1872,1664;Inherit;False;722;358;Comment;5;132;222;131;69;224;COLOR;1,0.8533139,0,1;0;0
Node;AmplifyShaderEditor.LerpOp;48;-1552,3504;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;128;-2690.605,2403.908;Inherit;False;SecondColor;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SamplerNode;80;96,5072;Inherit;True;Property;_TextureSample0;Texture Sample 0;18;0;Create;True;0;0;False;0;False;-1;0b6620ec919af2742b010c16b971678a;0b6620ec919af2742b010c16b971678a;True;0;True;bump;Auto;True;Instance;159;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;204;-2752,3648;Inherit;False;VertexOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;19;96,4880;Inherit;True;Property;_WaxNormal1;WaxNormal1;18;0;Create;True;0;0;False;0;False;-1;0b6620ec919af2742b010c16b971678a;0b6620ec919af2742b010c16b971678a;True;0;True;bump;Auto;True;Instance;159;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;297;-1504,3696;Inherit;False;295;SpecularOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;212;-1872,2864;Inherit;False;742.2822;414.7401;Comment;5;207;208;17;210;298;VEREXE_OFFSET;0.6367924,0.9299861,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;127;-2690.605,2227.908;Inherit;False;FirstColor;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;66;-336,5744;Inherit;True;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.StaticSwitch;292;-336,6016;Inherit;False;Property;_EmissionNoise;EmissionNoise;26;0;Create;True;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;184;432,5152;Inherit;False;181;_OUT_FLOW_MASK_;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;23;432,4896;Inherit;False;Constant;_Vector1;Vector 1;5;0;Create;True;0;0;False;0;False;0,0,0.5;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;132;-1808,1808;Inherit;False;128;SecondColor;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;207;-1840,2912;Inherit;False;203;_OUT_HIGH_;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;67;-64,5920;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.BlendNormalsNode;81;432,5056;Inherit;False;0;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;222;-1808,1904;Inherit;False;203;_OUT_HIGH_;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;208;-1840,3152;Inherit;False;204;VertexOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;75;-1344,3536;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.NormalVertexDataNode;298;-1841.899,2992.781;Inherit;False;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;131;-1808,1712;Inherit;False;127;FirstColor;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;256;96,5920;Inherit;False;_OUT_EMISSION_;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMinOpNode;214;-1216,3536;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;260;-2925.939,4464;Inherit;False;879.426;973.3408;Comment;8;293;0;226;211;221;213;257;294;OUT;0,0,0,1;0;0
Node;AmplifyShaderEditor.LerpOp;22;688,5008;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;17;-1600,2976;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.LerpOp;69;-1568,1776;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;224;-1360,1776;Inherit;False;_OUT_COLOR_;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;210;-1440,2976;Inherit;False;_OUT_VERTEx_OFFSET_;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;220;-1088,3552;Inherit;False;_OUT_SMOOTHNESS_;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;201;848,5008;Inherit;False;_OUT_NORMAL_;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.Vector3Node;294;-2832,4640;Inherit;False;Constant;_Vector0;Vector 0;26;0;Create;True;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;257;-2879.614,4796.158;Inherit;False;256;_OUT_EMISSION_;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.StaticSwitch;293;-2641.536,4703.945;Inherit;False;Property;_EmissionEnable;EmissionEnable;7;0;Create;True;0;0;False;0;False;0;1;1;True;;Toggle;2;Key0;Key1;Create;True;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SamplerNode;159;-3010.605,3139.908;Inherit;True;Property;_Normal;Normal;18;0;Create;True;0;0;False;0;False;-1;None;2efcbf7a1ef07a9459d0e31ffe7f327a;True;0;True;bump;Auto;True;Object;-1;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;226;-2641.676,4528;Inherit;False;224;_OUT_COLOR_;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;221;-2641.676,4800;Inherit;False;220;_OUT_SMOOTHNESS_;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;158;-3010.605,2947.908;Inherit;True;Property;_High;High;13;0;Create;True;0;0;False;0;False;-1;None;98c6f6f79eec0914c980ecc75476e9d8;True;0;False;black;Auto;False;Object;-1;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;213;-2641.676,4608;Inherit;False;201;_OUT_NORMAL_;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;211;-2641.676,4960;Inherit;False;210;_OUT_VERTEx_OFFSET_;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.StandardSurfaceOutputNode;0;-2305.676,4544;Float;False;True;-1;6;ASEMaterialInspector;0;0;Standard;CandleFlamesShaders/WaxQueue_Tessellation;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;False;False;False;False;False;False;Back;0;False;-1;0;False;-1;False;0;False;-1;0;False;-1;False;0;Opaque;0.5;True;True;0;False;Opaque;;Geometry;All;14;all;True;True;True;True;0;False;-1;False;0;False;-1;255;False;-1;255;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;True;2;14.5;10;25;False;0.5;True;0;5;False;-1;10;False;-1;0;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;0;0,0,0,0;VertexOffset;True;False;Cylindrical;False;Relative;0;CandleFlamesShaders/WaxQueue;-1;-1;-1;0;0;False;0;0;False;-1;-1;0;False;-1;0;0;0;False;0.1;False;-1;0;False;-1;16;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT;0;False;9;FLOAT;0;False;10;FLOAT;0;False;13;FLOAT3;0,0,0;False;11;FLOAT3;0,0,0;False;12;FLOAT3;0,0,0;False;14;FLOAT4;0,0,0,0;False;15;FLOAT3;0,0,0;False;0
WireConnection;148;0;142;0
WireConnection;4;0;147;0
WireConnection;43;0;25;2
WireConnection;44;0;43;0
WireConnection;154;0;4;0
WireConnection;187;0;185;1
WireConnection;187;1;185;2
WireConnection;264;1;266;0
WireConnection;264;0;265;1
WireConnection;45;0;44;0
WireConnection;163;0;162;1
WireConnection;163;1;162;2
WireConnection;189;0;187;0
WireConnection;165;0;163;0
WireConnection;5;1;156;0
WireConnection;47;0;45;0
WireConnection;47;1;25;2
WireConnection;47;2;264;0
WireConnection;194;0;5;0
WireConnection;194;1;191;0
WireConnection;267;0;47;0
WireConnection;181;0;267;0
WireConnection;3;0;167;0
WireConnection;3;1;194;0
WireConnection;198;0;196;0
WireConnection;2;1;3;0
WireConnection;11;0;195;0
WireConnection;11;1;2;1
WireConnection;50;0;11;0
WireConnection;50;2;199;0
WireConnection;259;0;50;0
WireConnection;232;0;231;0
WireConnection;150;0;149;0
WireConnection;203;0;259;0
WireConnection;245;0;52;2
WireConnection;79;0;147;0
WireConnection;79;1;151;0
WireConnection;55;0;227;0
WireConnection;55;1;233;0
WireConnection;78;0;79;0
WireConnection;54;0;245;0
WireConnection;54;1;55;0
WireConnection;234;0;228;0
WireConnection;186;0;185;3
WireConnection;186;1;185;4
WireConnection;243;0;54;0
WireConnection;58;0;245;0
WireConnection;155;0;78;0
WireConnection;246;0;60;0
WireConnection;247;0;64;0
WireConnection;164;0;162;3
WireConnection;164;1;162;4
WireConnection;57;0;58;0
WireConnection;57;1;243;0
WireConnection;188;0;186;0
WireConnection;237;0;236;0
WireConnection;174;0;172;0
WireConnection;166;0;164;0
WireConnection;77;1;157;0
WireConnection;175;0;173;0
WireConnection;62;0;57;0
WireConnection;62;1;235;0
WireConnection;250;0;247;0
WireConnection;249;0;246;0
WireConnection;193;0;77;0
WireConnection;193;1;192;0
WireConnection;263;0;62;0
WireConnection;137;0;133;0
WireConnection;183;0;182;0
WireConnection;138;0;134;0
WireConnection;280;0;238;0
WireConnection;280;1;291;0
WireConnection;295;0;296;0
WireConnection;65;0;252;0
WireConnection;65;1;253;0
WireConnection;65;2;263;0
WireConnection;215;0;44;0
WireConnection;129;0;123;0
WireConnection;28;0;182;0
WireConnection;28;1;176;0
WireConnection;130;0;124;0
WireConnection;82;0;183;0
WireConnection;82;1;177;0
WireConnection;76;0;168;0
WireConnection;76;1;193;0
WireConnection;48;0;140;0
WireConnection;48;1;141;0
WireConnection;48;2;217;0
WireConnection;128;0;130;0
WireConnection;80;1;76;0
WireConnection;80;5;82;0
WireConnection;204;0;205;0
WireConnection;19;1;3;0
WireConnection;19;5;28;0
WireConnection;127;0;129;0
WireConnection;66;0;65;0
WireConnection;66;1;263;0
WireConnection;292;1;238;0
WireConnection;292;0;280;0
WireConnection;67;0;66;0
WireConnection;67;1;292;0
WireConnection;81;0;19;0
WireConnection;81;1;80;0
WireConnection;75;0;48;0
WireConnection;75;1;216;0
WireConnection;75;2;297;0
WireConnection;256;0;67;0
WireConnection;214;0;75;0
WireConnection;22;0;23;0
WireConnection;22;1;81;0
WireConnection;22;2;184;0
WireConnection;17;0;207;0
WireConnection;17;1;298;0
WireConnection;17;2;208;0
WireConnection;69;0;131;0
WireConnection;69;1;132;0
WireConnection;69;2;222;0
WireConnection;224;0;69;0
WireConnection;210;0;17;0
WireConnection;220;0;214;0
WireConnection;201;0;22;0
WireConnection;293;1;294;0
WireConnection;293;0;257;0
WireConnection;0;0;226;0
WireConnection;0;1;213;0
WireConnection;0;2;293;0
WireConnection;0;4;221;0
WireConnection;0;11;211;0
ASEEND*/
//CHKSM=185C992E3EE3B1B9DC3C34964D0AA9CB164A436E