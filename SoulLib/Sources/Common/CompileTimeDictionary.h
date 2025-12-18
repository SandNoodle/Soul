#pragma once

#include <type_traits>

namespace Soul
{
	namespace Detail
	{
		template <bool B, auto V>
		struct VBase
		{
		};

		template <auto V>
		struct VBase<true, V>
		{
			static constexpr auto k_value = V;
		};

		template <bool B, typename T>
		struct TBase
		{
		};

		template <typename T>
		struct TBase<true, T>
		{
			using Type = T;
		};

	}  // namespace Detail

	template <auto V1, auto V2>
	struct VVTranslation
	{
		static constexpr auto k_value1 = V1;
		static constexpr auto k_value2 = V2;
	};

	template <typename T1, typename T2>
	struct TTTranslation
	{
		using Type1 = T1;
		using Type2 = T2;
	};

	template <auto V1, typename T2>
	struct VTTranslation
	{
		static constexpr auto k_value = V1;
		using Type                    = T2;
	};

	template <typename T1, auto V2>
	struct TVTranslation
	{
		using Type                    = T1;
		static constexpr auto k_value = V2;
	};

	template <auto V, typename... Ts>
	struct VVDictionary : public Detail::VBase<V == Ts::k_value1, Ts::k_value2>...
	{
	};

	template <typename T, typename... Ts>
	struct TTDictionary : public Detail::TBase<std::is_same_v<T, typename Ts::Type1>, typename Ts::Type2>...
	{
	};

	template <typename T, typename... Ts>
	struct TVDictionary : public Detail::VBase<std::is_same_v<T, typename Ts::Type>, Ts::k_value>...
	{
	};

	template <auto V, typename... Ts>
	struct VTDictionary : public Detail::TBase<V == Ts::k_value, typename Ts::Type>...
	{
	};
}  // namespace Soul
