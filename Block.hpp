#pragma once
#include "inner/Reflection.hpp"
#include "inner/DataState.hpp"
#include "inner/Index.hpp"
#include "inner/Exceptions.hpp"
#include "inner/Utilities.hpp"
#include "inner/Allocator.hpp"

namespace Langulus::Anyness
{
	
	/// Predeclarations																			
	class Any;
	template<ReflectedData T>
	class TAny;
	
	class Map;
	template<ReflectedData K, ReflectedData V>
	class TMap;
	template<ReflectedData K, ReflectedData V>
	class THashMap;
	
	class Set;
	template<ReflectedData T>
	class TSet;
	
	class Bytes;
	class Text;
	class Path;
	
	template<ReflectedData T>
	class TOwned;
	template<ReflectedData T, bool REFERENCED>
	class TPointer;

	/// Compression types, analogous to zlib's											
	enum class Compression {
		Nothing = 0,
		Fastest = 1,
		Balanced = 5,
		Smallest = 9,
		
		Default = Fastest
	};
	
	/// Data can have a temporal phase														
	/// Temporal data phases are used extensively by Langulus, but not at all	
	/// in standalone use. Either way - overhead is literally two bits, so		
	/// I've not taken the initiative to remove them - use them as you wish		
	/// Two bits for free! Free bits, yo! Check Block::GetState() and				
	/// DataState::Phased and DataState::Future for the aforementioned 2 bits	
	/// Remember, these are free only if you're using Anyness as standalone		
	enum class Phase : int {
		Past = -1,
		Now = 0,
		Future = 1
	};
	
	
	///																							
	///	BLOCK																					
	///																							
	///	Wraps an allocated memory block; acts as base to all containers.	
	///	This is an inner structure, that doesn't reference any memory,		
	/// only provides the functionality to do so. Avoid handling Block		
	/// instances unless you know exactly what you're doing.						
	///																							
	class Block {
		LANGULUS(DEEP);

	protected:
		union { 
			#if LANGULUS_DEBUG()
				char* mRawChar;
			#endif
			// Raw pointer to first element inside the memory block	
			Byte* mRaw {};
			Byte** mRawSparse;
		};
	
		// The data state																
		DataState mState {DataState::Default};
		// Number of written instances inside memory block					
		Count mCount {};
		// Number of allocated instances in the memory block				
		Count mReserved {};
		// Type of the instances inside the memory block					
		DMeta mType {};
		// Pointer to the allocated block										
		// If entry is zero, then data is static								
		Entry* mEntry {};

	public:
		constexpr Block() noexcept = default;
		constexpr Block(const Block&) noexcept = default;
			
		Block(Block&&) noexcept;
		explicit constexpr Block(DMeta) noexcept;
		Block(DMeta, Count, const Byte*) noexcept;
		Block(DMeta, Count, Byte*) noexcept;
		constexpr Block(const DataState&, DMeta) noexcept;
		Block(const DataState&, DMeta, Count, const Byte*) noexcept;
		Block(const DataState&, DMeta, Count, Byte*) noexcept;
	
		template<ReflectedData T>
		NOD() static Block From(T) requires Sparse<T>;
		template<ReflectedData T>
		NOD() static Block From(T, Count) requires Sparse<T>;
		template<ReflectedData T>
		NOD() static Block From(T&) requires Dense<T>;
		template<ReflectedData T>
		NOD() static Block From();

		constexpr Block& operator = (const Block&) noexcept = default;
		Block& operator = (Block&&) noexcept;
			
		Block& TakeAuthority();
		void Optimize();
	
	public:
		void SetType(DMeta, bool);
		template<ReflectedData T>
		void SetType(bool);
	
		void SetPhase(const Phase) noexcept;
		void SetState(DataState) noexcept;
	
		NOD() constexpr const DMeta& GetType() const noexcept;
		NOD() constexpr const Count& GetCount() const noexcept;
		NOD() constexpr const Count& GetReserved() const noexcept;
		NOD() Count GetCountDeep() const noexcept;
		NOD() Count GetCountElementsDeep() const noexcept;
		NOD() constexpr bool IsAllocated() const noexcept;
		NOD() constexpr bool IsPast() const noexcept;
		NOD() constexpr bool IsFuture() const noexcept;
		NOD() constexpr bool IsNow() const noexcept;
		NOD() constexpr bool IsMissing() const noexcept;
		NOD() bool IsMissingDeep() const;
		NOD() constexpr bool IsUntyped() const noexcept;
		NOD() constexpr bool IsTypeConstrained() const noexcept;
		NOD() constexpr bool IsPhased() const noexcept;
		NOD() constexpr bool IsEncrypted() const noexcept;
		NOD() constexpr bool IsCompressed() const noexcept;
		NOD() constexpr bool IsConstant() const noexcept;
		NOD() constexpr bool IsStatic() const noexcept;
		NOD() bool IsAbstract() const noexcept;
		NOD() bool IsConstructible() const noexcept;
		NOD() constexpr bool IsOr() const noexcept;
		NOD() constexpr bool IsEmpty() const noexcept;
		NOD() constexpr bool IsValid() const noexcept;
		NOD() constexpr bool IsInvalid() const noexcept;
		NOD() bool IsDense() const;
		NOD() bool IsSparse() const;
		NOD() bool IsDeep() const;
		NOD() constexpr Phase GetPhase() const noexcept;
		NOD() bool CanFitPhase(const Phase&) const noexcept;
		NOD() bool CanFitState(const Block&) const noexcept;
		NOD() Count GetSize() const noexcept;
		NOD() bool IsConcatable(const Block&) const noexcept;
	
		NOD() bool IsInsertable(DMeta) const noexcept;
		template<ReflectedData T>
		NOD() bool IsInsertable() const noexcept;
	
		NOD() Token GetToken() const noexcept;
		NOD() Stride GetStride() const noexcept;
		NOD() Byte* GetRaw() noexcept;
		NOD() const Byte* GetRaw() const noexcept;
		NOD() const Byte* GetRawEnd() const noexcept;
		NOD() Byte** GetRawSparse() noexcept;
		NOD() const Byte* const* GetRawSparse() const noexcept;
		template<ReflectedData T>
		NOD() T* GetRawAs() noexcept;
		template<ReflectedData T>
		NOD() const T* GetRawAs() const noexcept;
		template<ReflectedData T>
		NOD() const T* GetRawEndAs() const noexcept;
		NOD() constexpr const DataState& GetState() const noexcept;
		NOD() constexpr DataState GetUnconstrainedState() const noexcept;
	
		NOD() bool operator == (const Block&) const noexcept;
		NOD() bool operator != (const Block&) const noexcept;
	
		NOD() Byte* At(const Offset& = 0);
		NOD() const Byte* At(const Offset& = 0) const;
	
		template<ReflectedData T>
		NOD() decltype(auto) Get(const Offset& = 0, const Offset& = 0);
		template<ReflectedData T>
		NOD() decltype(auto) Get(const Offset& = 0, const Offset& = 0) const;
	
		template<ReflectedData T>
		NOD() decltype(auto) As(const Offset& = 0);
		template<ReflectedData T>
		NOD() decltype(auto) As(const Offset& = 0) const;
	
		template<ReflectedData T>
		NOD() decltype(auto) As(const Index&);
		template<ReflectedData T>
		NOD() decltype(auto) As(const Index&) const;
	
		template<ReflectedData T, bool FATAL_FAILURE = true>
		NOD() T AsCast(Index) const;
		template<ReflectedData T, bool FATAL_FAILURE = true>
		NOD() T AsCast() const;
	
		NOD() Block GetElementDense(Offset);
		NOD() const Block GetElementDense(Offset) const;
	
		NOD() Block GetElementResolved(Offset);
		NOD() const Block GetElementResolved(Offset) const;
	
		NOD() Block GetElement(Offset) noexcept;
		NOD() const Block GetElement(Offset) const noexcept;
	
		NOD() Block* GetBlockDeep(Offset) noexcept;
		NOD() const Block* GetBlockDeep(Offset) const noexcept;
	
		NOD() Block GetElementDeep(Offset) noexcept;
		NOD() const Block GetElementDeep(Offset) const noexcept;
	
		Count ForEachElement(TFunctor<bool(const Block&)>&&) const;
		Count ForEachElement(TFunctor<bool(Block&)>&&);
		Count ForEachElement(TFunctor<void(const Block&)>&&) const;
		Count ForEachElement(TFunctor<void(Block&)>&&);
	
		template<bool MUTABLE = true, class FUNCTION>
		Count ForEach(FUNCTION&&);
		template<class FUNCTION>
		Count ForEach(FUNCTION&&) const;
	
		template<bool MUTABLE = true, class FUNCTION>
		Count ForEachRev(FUNCTION&&);
		template<class FUNCTION>
		Count ForEachRev(FUNCTION&&) const;
	
		template<bool SKIP_DEEP_OR_EMPTY = true, bool MUTABLE = true, class FUNCTION>
		Count ForEachDeep(FUNCTION&&);
		template<bool SKIP_DEEP_OR_EMPTY = true, class FUNCTION>
		Count ForEachDeep(FUNCTION&&) const;
	
		template<bool SKIP_DEEP_OR_EMPTY = true, bool MUTABLE = true, class FUNCTION>
		Count ForEachDeepRev(FUNCTION&&);
		template<bool SKIP_DEEP_OR_EMPTY = true, class FUNCTION>
		Count ForEachDeepRev(FUNCTION&&) const;
	
	private:
		template<class RETURN, ReflectedData ARGUMENT, bool REVERSE, bool MUTABLE = true>
		Count ForEachInner(TFunctor<RETURN(ARGUMENT)>&&);
		template<class RETURN, ReflectedData ARGUMENT, bool REVERSE>
		Count ForEachInner(TFunctor<RETURN(ARGUMENT)>&&) const;
	
		template<class RETURN, ReflectedData ARGUMENT, bool REVERSE, bool SKIP_DEEP_OR_EMPTY, bool MUTABLE = true>
		Count ForEachDeepInner(TFunctor<RETURN(ARGUMENT)>&&);
		template<class RETURN, ReflectedData ARGUMENT, bool REVERSE, bool SKIP_DEEP_OR_EMPTY>
		Count ForEachDeepInner(TFunctor<RETURN(ARGUMENT)>&&) const;
	
		/// This function declaration is used to decompose a lambda				
		/// You can use it to extract the argument type of the lambda, using	
		/// decltype on the return type. Useful for template deduction in		
		/// the ForEach functions above, purely for convenience					
		template<typename RETURN, typename FUNCTION, typename ARGUMENT>
		ARGUMENT GetLambdaArgument(RETURN(FUNCTION::*)(ARGUMENT) const) const;
	
		NOD() Block CropInner(Offset, Count);
	
	public:
		NOD() bool Owns(const void*) const noexcept;
		NOD() bool CheckJurisdiction() const;
		NOD() bool CheckUsage() const;
		NOD() Count GetReferences() const;
		NOD() Block Crop(Offset, Count);
		NOD() Block Crop(Offset, Count) const;
	
		NOD() const Block GetMember(const Member&) const;
		NOD() Block GetMember(const Member&);
		NOD() const Block GetMember(TMeta, Offset = 0) const;
		NOD() Block GetMember(TMeta, Offset = 0);
		NOD() const Block GetMember(DMeta, Offset = 0) const;
		NOD() Block GetMember(DMeta, Offset = 0);
		NOD() const Block GetMember(std::nullptr_t, Offset = 0) const;
		NOD() Block GetMember(std::nullptr_t, Offset = 0);
	
		NOD() Block GetBaseMemory(DMeta, const Base&) const;
		NOD() Block GetBaseMemory(DMeta, const Base&);
		NOD() Block GetBaseMemory(const Base&) const;
		NOD() Block GetBaseMemory(const Base&);
	
		NOD() bool Mutate(DMeta);
		template<ReflectedData T>
		NOD() bool Mutate();
	
		void ToggleState(const DataState&, bool toggle = true);
		Block& MakeMissing();
		Block& MakeStatic();
		Block& MakeConstant();
		Block& MakeTypeConstrained();
		Block& MakeOr();
		Block& MakeAnd();
		Block& MakePast();
		Block& MakeFuture();
	
		Count Copy(Block&, bool allocate = false) const;
		Count Clone(Block&) const;
	
		NOD() bool CompareMembers(const Block&, Count& compared) const;
		NOD() bool CompareStates(const Block&) const noexcept;
		NOD() bool Compare(const Block&, bool resolve = true) const;
	
		void Allocate(Count, bool construct = false, bool setcount = false);
	
		template<ReflectedData T>
		void Allocate(Count, bool construct = false, bool setcount = false);
	
		void Extend(Count, bool construct = false, bool setcount = false);
		void Shrink(Count);
	
		Stride Compress(Block&, Compression = Compression::Default) const;
		Stride Decompress(Block&) const;
	
		Stride Encrypt(Block&, const Hash*, const Count&) const;
		Stride Decrypt(Block&, const Hash*, const Count&) const;
	
		NOD() Hash GetHash() const;
	
		template<ReflectedData T>
		NOD() Index Find(const T&, const Index& = Index::Front) const;
	
		template<ReflectedData T>
		NOD() Index FindDeep(const T&, const Index& = Index::Front) const;
		NOD() Index FindRTTI(const Block&, const Index& = Index::Front) const;
	
		Count Gather(Block&, Index = Index::Front) const;
		Count Gather(Block&, Phase, Index = Index::Front) const;
	
		template<ReflectedData T>
		void Swap(Offset, Offset);
	
		template<ReflectedData T>
		void Swap(Index, Index);
	
		template<ReflectedData T, bool MUTABLE = true>
		Count Emplace(T&&, const Index& = Index::Back);
	
		template<ReflectedData T, bool MUTABLE = true>
		Count Insert(const T*, Count = 1, const Index& = Index::Back);
		Count InsertBlock(const Block&, const Index& = Index::Back);
		Count InsertBlock(Block&&, const Index& = Index::Back);
	
		template<ReflectedData T>
		Count SmartPush(const T&, DataState = {}
			, bool attemptConcat = true
			, bool attemptDeepen = true
			, Index = Index::Back
		);
	
		template<Deep T>
		T& Deepen(bool moveState = true);
	
		template<ReflectedData T>
		Block& operator << (const T&);
	
		template<ReflectedData T>
		Block& operator << (T&);
	
		template<ReflectedData T>
		Block& operator << (T&&);
	
		template<ReflectedData T>
		Block& operator >> (const T&);
	
		template<ReflectedData T>
		Block& operator >> (T&); 
	
		template<ReflectedData T>
		Block& operator >> (T&&);
	
		template<ReflectedData T, bool MUTABLE = true>
		Count Merge(const T*, Count = 1, const Index& = Index::Back);
		Count MergeBlock(const Block&, const Index& = Index::Back);
	
		template<ReflectedData T>
		Block& operator <<= (const T&);
	
		template<ReflectedData T>
		Block& operator >>= (const T&);
	
		template<ReflectedData T>
		Count Remove(const T*, Count = 1, const Index& = Index::Front);
		Count RemoveIndex(const Index&, Count = 1);
		Count RemoveIndex(Offset, Count = 1);
		Count RemoveIndexDeep(Offset);
	
		Block& Trim(Offset);
	
		NOD() constexpr Index Constrain(const Index&) const noexcept;
	
		template<ReflectedData T>
		NOD() Index ConstrainMore(const Index&) const noexcept;
	
		template<ReflectedData T>
		NOD() Index GetIndexMax() const noexcept requires Sortable<T>;
	
		template<ReflectedData T>
		NOD() Index GetIndexMin() const noexcept requires Sortable<T>;
	
		template<ReflectedData T>
		NOD() Index GetIndexMode(Count&) const noexcept;
	
		template<ReflectedData T>
		void Sort(const Index&) noexcept;
	
		NOD() bool CanFit(DMeta) const;
		NOD() bool CanFit(const Block&) const;
	
		template<ReflectedData T>
		NOD() bool CanFit() const;
	
		NOD() bool InterpretsAs(DMeta) const;
	
		template<ReflectedData T>
		NOD() bool InterpretsAs() const;
	
		NOD() bool InterpretsAs(DMeta, Count) const;
	
		template<ReflectedData T>
		NOD() bool InterpretsAs(Count) const;
	
		NOD() bool Is(DMeta) const noexcept;
	
		template<ReflectedData T>
		NOD() bool Is() const;
	
	protected:
		constexpr void ClearInner() noexcept;
		template<bool TYPED>
		constexpr void ResetInner() noexcept;
	
		void Reference(const Count&) noexcept;
		template<bool DESTROY = true>
		bool Dereference(const Count&);
		void Keep();
		bool Free();
	
		void CallDefaultConstructors();
		void CallCopyConstructors(const Block&);
		void CallMoveConstructors(Block&&);
		void CallDestructors();
	
		Stride AllocateRegion(const Block&, const Index&, Block&);
	
		template<class FROM, class TO>
		static Count ConvertSymmetric(const Block&, Block&);
		template<class FROM>
		static Count ConvertDataBatched(const Block&, Block&, const Index&);
		template<class FROM>
		static Count ConvertToTextBlock(const Block&, Block&);
	};

		
	/// Macro used to implement the standard container interface used in		
	/// range-for-statements like for (auto& item : array)						
	/// In addition, a Reverse() function is added for reverse iteration		
	#define RANGED_FOR_INTEGRATION(containerType, iteratorType) \
		NOD() inline iteratorType* begin() noexcept { return GetRaw(); } \
		NOD() inline iteratorType* end() noexcept { return GetRawEnd(); } \
		NOD() inline iteratorType& last() noexcept { return *(GetRawEnd() - 1); } \
		NOD() inline const iteratorType* begin() const noexcept { return GetRaw(); } \
		NOD() inline const iteratorType* end() const noexcept { return GetRawEnd(); } \
		NOD() inline const iteratorType& last() const noexcept { return *(GetRawEnd() - 1); } \
		NOD() inline auto& size() const noexcept { return GetCount(); } \
		NOD() inline Inner::TReverse<const containerType, iteratorType> Reverse() const noexcept { return {*this}; } \
		NOD() inline Inner::TReverse<containerType, iteratorType> Reverse() noexcept { return {*this}; }

	namespace Inner
	{

		///																							
		/// Reverse adaptor for ranged-for expressions									
		///																							
		template<class T, class E>
		class TReverse {
		private:
			using ITERATOR = ::std::conditional_t<::std::is_const_v<T>, const E*, E*>;
		public:
			TReverse() = delete;
			TReverse(const TReverse&) = delete;
			TReverse(TReverse&&) = delete;
			TReverse(T&) noexcept;

			struct PointerWrapper {
				PointerWrapper() = delete;
				PointerWrapper(const PointerWrapper&) noexcept = default;
				PointerWrapper(PointerWrapper&&) noexcept = default;
				PointerWrapper(ITERATOR) noexcept;

				bool operator == (ITERATOR) const noexcept;
				bool operator != (ITERATOR) const noexcept;
				PointerWrapper& operator ++ () noexcept;
				PointerWrapper& operator -- () noexcept;
				operator ITERATOR () const noexcept;

				ITERATOR mPointer;
			};

			NOD() PointerWrapper begin() const noexcept;
			NOD() PointerWrapper end() const noexcept;
			NOD() E& last() const noexcept;
			NOD() Count size() const noexcept;

		private:
			T& mContainer;
		};

	} // namespace Langulus::Anyness::Inner
} // namespace Langulus::Anyness

#include "Block.inl"
