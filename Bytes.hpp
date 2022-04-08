#pragma once
#include "inner/Block.hpp"

namespace Langulus::Anyness
{

	///																								
	///	BYTE CONTAINER																			
	///																								
	/// Convenient wrapper for raw byte sequences										
	///																								
	class Bytes : public Inner::Block {
	public:
		Bytes();
		Bytes(const Bytes&);
		Bytes(Bytes&&) noexcept = default;

		Bytes(const Byte*, Count);

		template<Dense T>
		explicit Bytes(const T&) requires (pcIsPOD<T> || pcHasBase<T, InternalID>);

		~Bytes();

	public:
		NOD() Bytes Clone() const;
		NOD() Bytes Crop(Offset, Count) const;
		Bytes& Remove(Offset, Count);
		void Null(Count);
		void Clear() noexcept;
		void Reset();
		Bytes Extend(Count);
		Hash GetHash() const;

		Bytes& operator = (const Bytes&);
		Bytes& operator = (Bytes&&) noexcept;

		bool operator == (const Bytes&) const noexcept;
		bool operator != (const Bytes&) const noexcept;

		NOD() const Byte& operator[] (Index) const;
		NOD() Byte& operator[] (Index);

		Count Matches(const Bytes&) const noexcept;

		RANGED_FOR_INTEGRATION(Bytes, Byte);

		template<class T>
		Bytes& operator += (const T&);
	};

	/// Compile time check for text items													
	template<class T>
	concept IsBytes = pcHasBase<T, Bytes>;

	NOD() Bytes operator + (const Bytes&, const Bytes&);

	template<class T>
	NOD() Bytes operator + (const T&, const Bytes&) requires (!IsBytes<T>);

	template<class T>
	NOD() Bytes operator + (const Bytes&, const T&) requires (!IsBytes<T>);

} // namespace Langulus::Anyness

#include "Bytes.inl"
