///																									
/// Langulus::Anyness																			
/// Copyright(C) 2012 Dimo Markov <langulusteam@gmail.com>							
///																									
/// Distributed under GNU General Public License v3+									
/// See LICENSE file, or https://www.gnu.org/licenses									
///																									
#pragma once
#include "TAny.hpp"

namespace Langulus::Anyness
{

	///																								
	///	BYTE CONTAINER																			
	///																								
	/// Convenient wrapper for raw byte sequences										
	/// Can represent any POD type as a sequence of bytes								
	///																								
	class Bytes : public TAny<Byte> {
		LANGULUS(DEEP) false;		
	public:
		Bytes() = default;

		Bytes(const Bytes&);
		Bytes(Bytes&&) noexcept = default;

		Bytes(const TAny&);
		Bytes(TAny&&) noexcept;

		Bytes(const void*, const Size&);
		Bytes(void*, const Size&);
		
		Bytes(Disowned<Bytes>&&) noexcept;
		Bytes(Abandoned<Bytes>&&) noexcept;

		template<CT::POD T>
		Bytes(const T&);
		Bytes(const Token&);
		Bytes(const RTTI::Meta*);
		
		~Bytes();

		Bytes& operator = (const Bytes&);
		Bytes& operator = (Bytes&&) noexcept;

		Bytes& operator = (Disowned<Bytes>&&);
		Bytes& operator = (Abandoned<Bytes>&&) noexcept;
		
	public:
		NOD() Bytes Clone() const;
		NOD() Bytes Crop(const Offset&, const Count&) const;
		NOD() Bytes Crop(const Offset&, const Count&);
		Bytes& Remove(const Offset&, const Count&);
		void Null(const Count&);
		Bytes Extend(const Count&);
		Hash GetHash() const;

		bool operator == (const Bytes&) const noexcept;
		bool operator != (const Bytes&) const noexcept;

		template<class RHS>
		Bytes& operator += (const RHS&);
		template<class RHS>
		NOD() Bytes operator + (const RHS&) const;
	};

} // namespace Langulus::Anyness

#include "Bytes.inl"
