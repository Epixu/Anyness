/// Loosely based on martinus's robin-hood-hashing project                    
/// https://github.com/martinus/robin-hood-hashing                            
///                                                                           
#pragma once
#include "Map.hpp"
#include "Iterator.hpp"
#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <limits>
#include <memory> // only to support hash of smart pointers
#include <stdexcept>
#include <string>
#include <type_traits>
#include <utility>
#include <string_view>


namespace Langulus::Anyness
{
	namespace Inner
	{

		template <typename T>
		constexpr T rotr(T x, unsigned k) {
			return (x >> k) | (x << (8U * sizeof(T) - k));
		}

		// This cast gets rid of warnings like "cast from 'uint8_t*' {aka 'unsigned char*'} to
		// 'uint64_t*' {aka 'long unsigned int*'} increases required alignment of target type". Use with
		// care!
		template <typename T>
		inline T reinterpret_cast_no_cast_align_warning(void* ptr) noexcept {
			return reinterpret_cast<T>(ptr);
		}

		template <typename T>
		inline T reinterpret_cast_no_cast_align_warning(void const* ptr) noexcept {
			return reinterpret_cast<T>(ptr);
		}

		// make sure this is not inlined as it is slow and dramatically enlarges code, thus making other
		// inlinings more difficult. Throws are also generally the slow path.
		template <typename E, typename... Args>
		[[noreturn]] LANGULUS(NOINLINE) void doThrow(Args&&... args) {
			throw E(std::forward<Args>(args)...);
		}

		template <typename E, typename T, typename... Args>
		T* assertNotNull(T* t, Args&&... args) {
			if (LANGULUS_UNLIKELY(nullptr == t))
				doThrow<E>(std::forward<Args>(args)...);
			return t;
		}

	} // namespace Langulus::Anyness::Inner

	struct is_transparent_tag {};


	inline size_t hash_bytes(void const* ptr, size_t len) noexcept {
		static constexpr uint64_t m = UINT64_C(0xc6a4a7935bd1e995);
		static constexpr uint64_t seed = UINT64_C(0xe17a1465);
		static constexpr unsigned int r = 47;

		auto const* const data64 = static_cast<uint64_t const*>(ptr);
		uint64_t h = seed ^ (len * m);

		size_t const n_blocks = len / 8;
		for (size_t i = 0; i < n_blocks; ++i) {
			auto k = unaligned_load<uint64_t>(data64 + i);

			k *= m;
			k ^= k >> r;
			k *= m;

			h ^= k;
			h *= m;
		}

		auto const* const data8 = reinterpret_cast<uint8_t const*>(data64 + n_blocks);
		switch (len & 7U) {
		case 7:
			h ^= static_cast<uint64_t>(data8[6]) << 48U;
			[[fallthrough]];
		case 6:
			h ^= static_cast<uint64_t>(data8[5]) << 40U;
			[[fallthrough]];
		case 5:
			h ^= static_cast<uint64_t>(data8[4]) << 32U;
			[[fallthrough]];
		case 4:
			h ^= static_cast<uint64_t>(data8[3]) << 24U;
			[[fallthrough]];
		case 3:
			h ^= static_cast<uint64_t>(data8[2]) << 16U;
			[[fallthrough]];
		case 2:
			h ^= static_cast<uint64_t>(data8[1]) << 8U;
			[[fallthrough]];
		case 1:
			h ^= static_cast<uint64_t>(data8[0]);
			h *= m;
			[[fallthrough]];
		default:
			break;
		}

		h ^= h >> r;

		// not doing the final step here, because this will be done by keyToIdx anyways
		// h *= m;
		// h ^= h >> r;
		return static_cast<size_t>(h);
	}

	inline size_t hash_int(uint64_t x) noexcept {
		// tried lots of different hashes, let's stick with murmurhash3. It's simple, fast, well tested,
		// and doesn't need any special 128bit operations.
		x ^= x >> 33U;
		x *= UINT64_C(0xff51afd7ed558ccd);
		x ^= x >> 33U;

		// not doing the final step here, because this will be done by keyToIdx anyways
		// x *= UINT64_C(0xc4ceb9fe1a85ec53);
		// x ^= x >> 33U;
		return static_cast<size_t>(x);
	}

	// A thin wrapper around std::hash, performing an additional simple mixing step of the result.
	template <typename T, typename Enable = void>
	struct hash : public std::hash<T> {
		size_t operator()(T const& obj) const
			noexcept(noexcept(std::declval<std::hash<T>>().operator()(std::declval<T const&>()))) {
			// call base hash
			auto result = std::hash<T>::operator()(obj);
			// return mixed of that, to be save against identity has
			return hash_int(static_cast<Pointer>(result));
		}
	};

	template <typename CharT>
	struct hash<std::basic_string<CharT>> {
		size_t operator()(std::basic_string<CharT> const& str) const noexcept {
			return hash_bytes(str.data(), sizeof(CharT) * str.size());
		}
	};

	template <typename CharT>
	struct hash<std::basic_string_view<CharT>> {
		size_t operator()(std::basic_string_view<CharT> const& sv) const noexcept {
			return hash_bytes(sv.data(), sizeof(CharT) * sv.size());
		}
	};

	template <class T>
	struct hash<T*> {
		size_t operator()(T* ptr) const noexcept {
			return hash_int(reinterpret_cast<Pointer>(ptr));
		}
	};

	template <class T>
	struct hash<std::unique_ptr<T>> {
		size_t operator()(std::unique_ptr<T> const& ptr) const noexcept {
			return hash_int(reinterpret_cast<Pointer>(ptr.get()));
		}
	};

	template <class T>
	struct hash<std::shared_ptr<T>> {
		size_t operator()(std::shared_ptr<T> const& ptr) const noexcept {
			return hash_int(reinterpret_cast<Pointer>(ptr.get()));
		}
	};

	template <typename Enum>
	struct hash<Enum, typename std::enable_if<std::is_enum<Enum>::value>::type> {
		size_t operator()(Enum e) const noexcept {
			using Underlying = typename std::underlying_type<Enum>::type;
			return hash<Underlying>{}(static_cast<Underlying>(e));
		}
	};

#define ROBIN_HOOD_HASH_INT(T)                           \
    template <>                                          \
    struct hash<T> {                                     \
        size_t operator()(T const& obj) const noexcept { \
            return hash_int(static_cast<uint64_t>(obj)); \
        }                                                \
    }

#if defined(__GNUC__) && !defined(__clang__)
#    pragma GCC diagnostic push
#    pragma GCC diagnostic ignored "-Wuseless-cast"
#endif

	// see https://en.cppreference.com/w/cpp/utility/hash
	ROBIN_HOOD_HASH_INT(bool);
	ROBIN_HOOD_HASH_INT(char);
	ROBIN_HOOD_HASH_INT(signed char);
	ROBIN_HOOD_HASH_INT(unsigned char);
	ROBIN_HOOD_HASH_INT(char16_t);
	ROBIN_HOOD_HASH_INT(char32_t);
	ROBIN_HOOD_HASH_INT(wchar_t);
	ROBIN_HOOD_HASH_INT(short);
	ROBIN_HOOD_HASH_INT(unsigned short);
	ROBIN_HOOD_HASH_INT(int);
	ROBIN_HOOD_HASH_INT(unsigned int);
	ROBIN_HOOD_HASH_INT(long);
	ROBIN_HOOD_HASH_INT(long long);
	ROBIN_HOOD_HASH_INT(unsigned long);
	ROBIN_HOOD_HASH_INT(unsigned long long);
#if defined(__GNUC__) && !defined(__clang__)
#    pragma GCC diagnostic pop
#endif

	namespace Inner
	{

		template<class T>
		struct void_type {
			using type = void;
		};


		template<class T>
		concept has_is_transparent = requires { T::is_transparent; };

		// using wrapper classes for hash and key_equal prevents the diamond problem when the same type
		// is used. see https://stackoverflow.com/a/28771920/48181
		template <typename T>
		struct WrapHash : public T {
			WrapHash() = default;
			explicit WrapHash(T const& o) noexcept(noexcept(T(std::declval<T const&>())))
				: T(o) {}
		};

		template <typename T>
		struct WrapKeyEqual : public T {
			WrapKeyEqual() = default;
			explicit WrapKeyEqual(T const& o) noexcept(noexcept(T(std::declval<T const&>())))
				: T(o) {}
		};

		template<class T>
		concept IsFlatMap = T::IsFlat;
		template<class T>
		concept IsMap = T::is_map;
		template<class T>
		concept IsSet = T::is_set;
		template<class T>
		concept IsTransparent = T::is_transparent;

		/// Type needs to be wider than uint8_t											
		using InfoType = uint32_t;

#define NODE_TEMPLATE()	template<class M, class T, class V>
#define NODE(a) DataNodeOn##a<M, T, V>

#define TABLE_TEMPLATE() template<bool IsFlat, size_t MaxLoadFactor100, class Key, class T>
#define TABLE() Table<IsFlat, MaxLoadFactor100, Key, T>


		/// DataNode																				
		/// Primary template for the data node. We have special						
		/// implementations for small and big objects. For large objects it		
		/// is assumed that swap() is fairly slow, so we allocate these on		
		/// the heap so swap merely swaps a pointer.										

		/// Small: just allocate on the stack												
		NODE_TEMPLATE()
		class DataNodeOnStack final {
		private:
			T mData;

		public:
			template <typename... Args>
			explicit DataNodeOnStack(M&, Args&&...args) noexcept(noexcept(T(std::forward<Args>(args)...)));
			DataNodeOnStack(M&, DataNodeOnStack&&) noexcept(std::is_nothrow_move_constructible<T>::value);

			void destroy(M&) noexcept {}
			void destroyDoNotDeallocate() noexcept {}

			T const* operator->() const noexcept;
			T* operator->() noexcept;
			const T& operator*() const noexcept;
			T& operator*() noexcept;

			template<typename VT = T>
			NOD() typename VT::first_type& getFirst() noexcept requires IsMap<M>;
			template<typename VT = T>
			NOD() VT& getFirst() noexcept requires IsSet<M>;
			template<typename VT = T>
			NOD() typename VT::first_type const& getFirst() const noexcept requires IsMap<M>;
			template<typename VT = T>
			NOD() VT const& getFirst() const noexcept requires IsSet<M>;

			template<typename MT = V>
			NOD() MT& getSecond() noexcept requires IsMap<M>;
			template<typename MT = V>
			NOD() MT const& getSecond() const noexcept requires IsSet<M>;

			void swap(DataNodeOnStack&) noexcept(noexcept(std::declval<T>().swap(std::declval<T>())));
		};


		/// Big object: allocate on heap														
		NODE_TEMPLATE()
		class DataNodeOnHeap {
		private:
			T* mData;

		public:
			template <typename... Args>
			explicit DataNodeOnHeap(M&, Args&&...);
			DataNodeOnHeap(M&, DataNodeOnHeap&&) noexcept;

			void destroy(M&) noexcept;
			void destroyDoNotDeallocate() noexcept;

			T const* operator->() const noexcept;
			T* operator->() noexcept;
			const T& operator*() const;
			T& operator*();

			template<typename VT = T>
			NOD() typename VT::first_type& getFirst() noexcept requires IsMap<M>;
			template<typename VT = T>
			NOD() VT& getFirst() noexcept requires IsSet<M>;
			template<typename VT = T>
			NOD() typename VT::first_type const& getFirst() const noexcept requires IsMap<M>;
			template<typename VT = T>
			NOD() VT const& getFirst() const noexcept requires IsSet<M>;

			template<typename MT = V>
			NOD() MT& getSecond() noexcept requires IsMap<M>;
			template<typename MT = V>
			NOD() MT const& getSecond() const noexcept requires IsMap<M>;

			void swap(DataNodeOnHeap&) noexcept;
		};


		/// A highly optimized hashmap implementation, using the Robin Hood		
		/// algorithm																				
		/// In most cases, this map should be usable as a drop-in replacement	
		/// for std::unordered_map, but be about 2x faster in most cases and		
		/// require much less allocations.													
		///																							
		/// This implementation uses the following memory layout:					
		///																							
		/// [Node, Node, ... Node | info, info, ... infoSentinel ]					
		///																							
		/// * Node: either a DataNode that directly has the std::pair<key, val>	
		///   as member, or a DataNode with a pointer to std::pair<key,val>.		
		///   Which DataNode representation to use depends on how fast the		
		///   swap() operation is. Heuristically, this is automatically choosen	
		/// 	based on sizeof(). there are always 2^n Nodes.							
		///																							
		/// * info: Each Node in the map has a corresponding info byte, so		
		///   there are 2^n info bytes. Each byte is initialized to 0, meaning	
		///   the corresponding Node is empty. Set to 1 means the corresponding	
		///   node contains data. Set to 2 means the corresponding Node is		
		///	filled, but it actually belongs to the previous position and was	
		///   pushed out because that place is already taken.							
		///																							
		/// * infoSentinel: Sentinel byte set to 1, so that iterator's ++ can	
		///	stop at end() without the need for a idx variable.						
		///																							
		/// According to STL, order of templates has effect on throughput.		
		/// That's why I've moved the boolean to the front.							
		/// https://www.reddit.com/r/cpp/comments/ahp6iu/compile_time_binary_size_reductions_and_cs_future/eeguck4/
		///																							
		TABLE_TEMPLATE()
		class Table
			: NodeAllocator<Conditional<IsVoid<T>, Key, TPair<Conditional<IsFlat, Key, Key const>, T>>, 4, 16384, IsFlat>
		{
		public:
			static constexpr bool is_flat = IsFlat;
			static constexpr bool is_map = not IsVoid<T>;
			static constexpr bool is_set = !is_map;
			//static constexpr bool is_transparent = has_is_transparent<Hash> && has_is_transparent<KeyEqual>;

			using PairInner = TPair<Conditional<IsFlat, Key, Key const>, T>;
			using Pair = TPair<Key, T>;
			using key_type = Key;
			using mapped_type = T;
			using value_type = Conditional<is_set, Key, PairInner>;
			using Self = TABLE();

		private:
			static_assert(MaxLoadFactor100 > 10 && MaxLoadFactor100 < 100,
				"MaxLoadFactor100 needs to be >10 && < 100");

			using Node = Conditional<
				IsFlat,
				DataNodeOnStack<Self, value_type, mapped_type>,
				DataNodeOnHeap<Self, value_type, mapped_type>
			>;

			// members are sorted so no padding occurs
			uint64_t mHashMultiplier = UINT64_C(0xc4ceb9fe1a85ec53);                // 8 byte  8
			Node* mKeyVals = reinterpret_cast_no_cast_align_warning<Node*>(&mMask); // 8 byte 16
			uint8_t* mInfo = reinterpret_cast<uint8_t*>(&mMask);                    // 8 byte 24
			size_t mNumElements = 0;                                                // 8 byte 32
			size_t mMask = 0;                                                       // 8 byte 40
			size_t mMaxNumElementsAllowed = 0;                                      // 8 byte 48
			InfoType mInfoInc = InitialInfoInc;                                     // 4 byte 52
			InfoType mInfoHashShift = InitialInfoHashShift;                         // 4 byte 56
			// 16 byte 56 if NodeAllocator

			//using WHash = WrapHash<Hash>;
			//using WKeyEqual = WrapKeyEqual<KeyEqual>;

			// configuration defaults

			// make sure we have 8 elements, needed to quickly rehash mInfo
			static constexpr size_t InitialNumElements = sizeof(uint64_t);
			static constexpr uint32_t InitialInfoNumBits = 5;
			static constexpr uint8_t InitialInfoInc = 1U << InitialInfoNumBits;
			static constexpr size_t InfoMask = InitialInfoInc - 1U;
			static constexpr uint8_t InitialInfoHashShift = 0;
			using DataPool = NodeAllocator<value_type, 4, 16384, IsFlat>;

		public:
			Table() noexcept;
			explicit Table(size_t) noexcept;

			template<typename Iter>
			Table(Iter, Iter, size_t = 0);
			Table(std::initializer_list<value_type>, size_t = 0);
			Table(Table&&) noexcept;
			Table(const Table&);
			~Table();

			Table& operator = (Table&&) noexcept;
			Table& operator = (const Table&);

			Table& operator = (Pair&&) noexcept;
			Table& operator = (const Pair&);

			DMeta GetKeyType() const;
			DMeta GetValueType() const;

			template<class ALT_T>
			bool KeyIs() const noexcept;
			template<class ALT_T>
			bool ValueIs() const noexcept;

			Table& operator << (Pair&&);
			Table& operator << (const Pair&);
			Table& operator >> (Pair&&);
			Table& operator >> (const Pair&);

			// helpers for insertKeyPrepareEmptySpot: extract first entry (only const required)
			NOD() key_type const& getFirstConst(Node const& n) const noexcept {
				return n.getFirst();
			}

			// in case we have void mapped_type, we are not using a pair, thus we just route k through.
			// No need to disable this because it's just not used if not applicable.
			NOD() key_type const& getFirstConst(key_type const& k) const noexcept {
				return k;
			}

			// in case we have non-void mapped_type, we have a standard robin_hood::pair
			template <ReflectedData Q = mapped_type>
			NOD() key_type const& getFirstConst(value_type const& vt) const noexcept {
				return vt.first;
			}

			/// Destroyer																			
			template<bool DEALLOCATE>
			void DestroyNodes() const noexcept {
				mNumElements = 0;

				if constexpr (!IsFlat || !std::is_trivially_destructible_v<Node>) {
					// Clear also resets mInfo to 0, that's sometimes not	
					// necessary														
					auto const numElementsWithBuffer = calcNumElementsWithBuffer(mMask + 1);
					for (size_t idx = 0; idx < numElementsWithBuffer; ++idx) {
						if (0 != mInfo[idx]) {
							Node& n = mKeyVals[idx];
							if constexpr (DEALLOCATE)
								n.destroy(*this);
							else
								n.destroyDoNotDeallocate();
							n.~Node();
						}
					}
				}
			}

			/// Highly performance relevant code											
			/// Lower bits are used for indexing into the array (2^n size)			
			/// The upper 1-5 bits need to be a reasonable good hash, to save		
			/// comparisons																		
			template<class HashKey>
			void keyToIdx(HashKey&& key, size_t* idx, InfoType* info) const {
				static_assert(IsHashable<HashKey>, "Contained key type is not hashable");
				// In addition to whatever hash is used, add another mul &	
				// shift so we get better hashing. This serves as a bad		
				// hash prevention, if the given data is badly mixed.			
				uint64_t h = static_cast<uint64_t>(key.GetHash());
				h *= mHashMultiplier;
				h ^= h >> 33U;

				// the lower InitialInfoNumBits are reserved for info.
				*info = mInfoInc + static_cast<InfoType>((h & InfoMask) >> mInfoHashShift);
				*idx = (static_cast<size_t>(h) >> InitialInfoNumBits) & mMask;
			}

			// forwards the index by one, wrapping around at the end
			void next(InfoType* info, size_t* idx) const noexcept {
				*idx = *idx + 1;
				*info += mInfoInc;
			}

			void nextWhileLess(InfoType* info, size_t* idx) const noexcept {
				// unrolling this by hand did not bring any speedups.
				while (*info < mInfo[*idx])
					next(info, idx);
			}

			// Shift everything up by one element. Tries to move stuff around.
			void shiftUp(size_t startIdx, size_t const insertion_idx) noexcept(std::is_nothrow_move_assignable_v<Node>) {
				auto idx = startIdx;
				::new (static_cast<void*>(mKeyVals + idx)) Node(std::move(mKeyVals[idx - 1]));
				while (--idx != insertion_idx)
					mKeyVals[idx] = std::move(mKeyVals[idx - 1]);

				idx = startIdx;
				while (idx != insertion_idx) {
					mInfo[idx] = static_cast<uint8_t>(mInfo[idx - 1] + mInfoInc);
					if (LANGULUS_UNLIKELY(mInfo[idx] + mInfoInc > 0xFF))
						mMaxNumElementsAllowed = 0;
					--idx;
				}
			}

			void shiftDown(size_t idx) noexcept(std::is_nothrow_move_assignable_v<Node>) {
				// until we find one that is either empty or has zero offset.
				// TODO(martinus) we don't need to move everything, just the last one for the same
				// bucket.
				mKeyVals[idx].destroy(*this);

				// until we find one that is either empty or has zero offset.
				while (mInfo[idx + 1] >= 2 * mInfoInc) {
					mInfo[idx] = static_cast<uint8_t>(mInfo[idx + 1] - mInfoInc);
					mKeyVals[idx] = std::move(mKeyVals[idx + 1]);
					++idx;
				}

				mInfo[idx] = 0;
				// don't destroy, we've moved it
				// mKeyVals[idx].destroy(*this);
				mKeyVals[idx].~Node();
			}

			/// Copy of find(), except that it returns iterator instead of			
			/// const_iterator																	
			template<class Other>
			NOD() size_t findIdx(Other const& key) const {
				static_assert(IsComparable<Other, Key>, "Can't compare keys");
				size_t idx {};
				InfoType info {};
				keyToIdx(key, &idx, &info);

				do {
					// Unrolling this twice gives a bit of a speedup			
					// More unrolling did not help									
					if (info == mInfo[idx] && LANGULUS_LIKELY(key == mKeyVals[idx].getFirst()))
						return idx;
					next(&info, &idx);
					if (info == mInfo[idx] && LANGULUS_LIKELY(key == mKeyVals[idx].getFirst()))
						return idx;
					next(&info, &idx);
				} while (info <= mInfo[idx]);

				// If reached, then nothing found									
				return mMask == 0 
					? 0
					: static_cast<size_t>(std::distance(mKeyVals, reinterpret_cast_no_cast_align_warning<Node*>(mInfo)));
			}

			void cloneData(const Table& o) {
				if constexpr (IsFlat && std::is_trivially_copyable_v<Node>) {
					auto const* const src = reinterpret_cast<char const*>(o.mKeyVals);
					auto* tgt = reinterpret_cast<char*>(mKeyVals);
					auto const numElementsWithBuffer = calcNumElementsWithBuffer(mMask + 1);
					std::copy(src, src + calcNumBytesTotal(numElementsWithBuffer), tgt);
				}
				else {
					auto const numElementsWithBuffer = calcNumElementsWithBuffer(mMask + 1);
					std::copy(o.mInfo, o.mInfo + calcNumBytesInfo(numElementsWithBuffer), mInfo);

					for (size_t i = 0; i < numElementsWithBuffer; ++i) {
						if (mInfo[i])
							::new (static_cast<void*>(mKeyVals + i)) Node(*this, *o.mKeyVals[i]);
					}
				}
			}

			/// Clone the table																	
			///	@return the new table														
			NOD() Table Clone() const {
				Table result;
				result.CloneData(*this);
				return result;
			}

			/// Inserts a keyval that is guaranteed to be new, e.g. when the		
			/// hashmap is resized																
			///	@return True on success, false if something went wrong			
			void insert_move(Node&& keyval) {
				// we don't retry, fail if overflowing
				// don't need to check max num elements
				if (0 == mMaxNumElementsAllowed && !try_increase_info())
					throwOverflowError();

				size_t idx {};
				InfoType info {};
				keyToIdx(keyval.getFirst(), &idx, &info);

				// skip forward. Use <= because we are certain that the element is not there.
				while (info <= mInfo[idx]) {
					idx = idx + 1;
					info += mInfoInc;
				}

				// key not found, so we are now exactly where we want to insert it.
				auto const insertion_idx = idx;
				auto const insertion_info = static_cast<uint8_t>(info);
				if (LANGULUS_UNLIKELY(insertion_info + mInfoInc > 0xFF))
					mMaxNumElementsAllowed = 0;

				// find an empty spot
				while (0 != mInfo[idx]) {
					next(&info, &idx);
				}

				auto& l = mKeyVals[insertion_idx];
				if (idx == insertion_idx) {
					::new (static_cast<void*>(&l)) Node(std::move(keyval));
				}
				else {
					shiftUp(idx, insertion_idx);
					l = std::move(keyval);
				}

				// put at empty spot
				mInfo[insertion_idx] = insertion_info;

				++mNumElements;
			}

		public:
			using iterator = Iterator<false, Self>;
			using const_iterator = Iterator<true, Self>;



			/// Swaps everything between the two maps										
			void swap(Table& o) {
				std::swap(o, *this);
			}

			/// Clears all data, without resizing											
			void Clear() {
				if (IsEmpty()) {
					// Don't do anything! also important because we don't		
					// want to write to DummyInfoByte::b, even though we		
					// would just write 0 to it.										
					return;
				}

				DestroyNodes<true>();

				auto const numElementsWithBuffer = calcNumElementsWithBuffer(mMask + 1);

				// Clear everything, then set the sentinel again				
				uint8_t const z = 0;
				std::fill(mInfo, mInfo + calcNumBytesInfo(numElementsWithBuffer), z);
				mInfo[numElementsWithBuffer] = 1;

				mInfoInc = InitialInfoInc;
				mInfoHashShift = InitialInfoHashShift;
			}

			/// Checks if both tables contain the same entries							
			/// Order is irrelevant																
			bool operator == (const Table& other) const {
				if (other.GetCount() != GetCount())
					return false;

				for (auto const& otherEntry : other) {
					if (!has(otherEntry))
						return false;
				}

				return true;
			}

			/// Checks if both tables contain different entries						
			/// Order is irrelevant																
			bool operator != (const Table& other) const {
				return !operator == (other);
			}

			/// Access value by key																
			///	@param key - the key to find												
			///	@return a reference to the value											
			template <ReflectedData Q = mapped_type>
			Q& operator[] (const key_type& key) {
				auto idxAndState = insertKeyPrepareEmptySpot(key);
				switch (idxAndState.second) {
				case InsertionState::key_found:
					break;

				case InsertionState::new_node:
					::new (static_cast<void*>(&mKeyVals[idxAndState.first])) 
						Node(*this, std::piecewise_construct, std::forward_as_tuple(key), std::forward_as_tuple());
					break;

				case InsertionState::overwrite_node:
					mKeyVals[idxAndState.first] = 
						Node(*this, std::piecewise_construct, std::forward_as_tuple(key), std::forward_as_tuple());
					break;

				case InsertionState::overflow_error:
					throwOverflowError();
				}

				return mKeyVals[idxAndState.first].getSecond();
			}

			/// Access value by key																
			///	@param key - the key to find												
			///	@return a reference to the value											
			template <ReflectedData Q = mapped_type>
			Q& operator[] (key_type&& key) {
				auto idxAndState = insertKeyPrepareEmptySpot(key);
				switch (idxAndState.second) {
				case InsertionState::key_found:
					break;

				case InsertionState::new_node:
					::new (static_cast<void*>(&mKeyVals[idxAndState.first])) 
						Node(*this, std::piecewise_construct, std::forward_as_tuple(Move(key)), std::forward_as_tuple());
					break;

				case InsertionState::overwrite_node:
					mKeyVals[idxAndState.first] = 
						Node(*this, std::piecewise_construct, std::forward_as_tuple(Move(key)), std::forward_as_tuple());
					break;

				case InsertionState::overflow_error:
					throwOverflowError();
				}

				return mKeyVals[idxAndState.first].getSecond();
			}

			/// Insert a number of items														
			///	@param first - the first element											
			///	@param last - the last element											
			template <typename Iter>
			void insert(Iter first, Iter last) {
				for (; first != last; ++first) {
					// value_type ctor needed because this might be called	
					// with std::pair's													
					insert(value_type(*first));
				}
			}

			/// Insert a number of items via initializer list							
			///	@param ilist - the first element											
			void insert(std::initializer_list<value_type> ilist) {
				for (auto&& vt : ilist) {
					insert(Move(vt));
				}
			}

			/// Emplace an items inside map													
			///	@param ...args - items to add												
			///	@return 
			template<typename... Args>
			std::pair<iterator, bool> Emplace(Args&&... args) {
				Node n {*this, Forward<Args>(args)...};

				auto idxAndState = insertKeyPrepareEmptySpot(getFirstConst(n));
				switch (idxAndState.second) {
				case InsertionState::key_found:
					n.destroy(*this);
					break;

				case InsertionState::new_node:
					::new (static_cast<void*>(&mKeyVals[idxAndState.first]))
						Node(*this, Move(n));
					break;

				case InsertionState::overwrite_node:
					mKeyVals[idxAndState.first] = Move(n);
					break;

				case InsertionState::overflow_error:
					n.destroy(*this);
					throwOverflowError();
					break;
				}

				return std::make_pair(
					iterator(mKeyVals + idxAndState.first, mInfo + idxAndState.first),
					InsertionState::key_found != idxAndState.second
				);
			}

			template <typename... Args>
			iterator emplace_hint(const_iterator position, Args&&... args) {
				(void) position;
				return emplace(std::forward<Args>(args)...).first;
			}

			template <typename... Args>
			std::pair<iterator, bool> try_emplace(const key_type& key, Args&&... args) {
				return try_emplace_impl(key, std::forward<Args>(args)...);
			}

			template <typename... Args>
			std::pair<iterator, bool> try_emplace(key_type&& key, Args&&... args) {
				return try_emplace_impl(std::move(key), std::forward<Args>(args)...);
			}

			template <typename... Args>
			iterator try_emplace(const_iterator hint, const key_type& key, Args&&... args) {
				(void) hint;
				return try_emplace_impl(key, std::forward<Args>(args)...).first;
			}

			template <typename... Args>
			iterator try_emplace(const_iterator hint, key_type&& key, Args&&... args) {
				(void) hint;
				return try_emplace_impl(std::move(key), std::forward<Args>(args)...).first;
			}

			template <typename Mapped>
			std::pair<iterator, bool> insert_or_assign(const key_type& key, Mapped&& obj) {
				return insertOrAssignImpl(key, std::forward<Mapped>(obj));
			}

			template <typename Mapped>
			std::pair<iterator, bool> insert_or_assign(key_type&& key, Mapped&& obj) {
				return insertOrAssignImpl(std::move(key), std::forward<Mapped>(obj));
			}

			template <typename Mapped>
			iterator insert_or_assign(const_iterator hint, const key_type& key, Mapped&& obj) {
				(void) hint;
				return insertOrAssignImpl(key, std::forward<Mapped>(obj)).first;
			}

			template <typename Mapped>
			iterator insert_or_assign(const_iterator hint, key_type&& key, Mapped&& obj) {
				(void) hint;
				return insertOrAssignImpl(std::move(key), std::forward<Mapped>(obj)).first;
			}

			std::pair<iterator, bool> insert(const value_type& keyval) {
				return emplace(keyval);
			}

			iterator insert(const_iterator hint, const value_type& keyval) {
				(void) hint;
				return emplace(keyval).first;
			}

			std::pair<iterator, bool> insert(value_type&& keyval) {
				return emplace(std::move(keyval));
			}

			iterator insert(const_iterator hint, value_type&& keyval) {
				(void) hint;
				return emplace(std::move(keyval)).first;
			}

			/// Returns 1 if key is found, 0 otherwise									
			size_t count(const key_type& key) const {
				auto kv = mKeyVals + findIdx(key);
				if (kv != reinterpret_cast_no_cast_align_warning<Node*>(mInfo))
					return 1;
				return 0;
			}

			template <typename OtherKey, typename Self_ = Self>
			size_t count(const OtherKey& key) const requires IsTransparent<Self_> {
				auto kv = mKeyVals + findIdx(key);
				if (kv != reinterpret_cast_no_cast_align_warning<Node*>(mInfo))
					return 1;
				return 0;
			}

			bool contains(const key_type& key) const {
				return 1U == count(key);
			}

			template <typename OtherKey, typename Self_ = Self>
			bool contains(const OtherKey& key) const requires IsTransparent<Self_> {
				return 1U == count(key);
			}

			/// Returns a reference to the value found for key							
			/// Throws std::out_of_range if element cannot be found					
			template <ReflectedData Q = mapped_type>
			Q& at(key_type const& key) {
				auto kv = mKeyVals + findIdx(key);
				if (kv == reinterpret_cast_no_cast_align_warning<Node*>(mInfo))
					doThrow<std::out_of_range>("key not found");
				return kv->getSecond();
			}

			/// Returns a reference to the value found for key							
			/// Throws std::out_of_range if element cannot be found					
			template <ReflectedData Q = mapped_type>
			Q const& at(key_type const& key) const {
				auto kv = mKeyVals + findIdx(key);
				if (kv == reinterpret_cast_no_cast_align_warning<Node*>(mInfo))
					doThrow<std::out_of_range>("key not found");
				return kv->getSecond();
			}

			/// Find																					
			const_iterator find(const key_type& key) const {
				const size_t idx = findIdx(key);
				return const_iterator {mKeyVals + idx, mInfo + idx};
			}

			/*template <typename OtherKey>
			const_iterator find(const OtherKey& key, is_transparent_tag) const {
				const size_t idx = findIdx(key);
				return const_iterator {mKeyVals + idx, mInfo + idx};
			}

			template <typename OtherKey, typename Self_ = Self>
			const_iterator find(const OtherKey& key) const requires IsTransparent<Self_> {
				const size_t idx = findIdx(key);
				return const_iterator {mKeyVals + idx, mInfo + idx};
			}*/

			iterator find(const key_type& key) {
				const size_t idx = findIdx(key);
				return iterator {mKeyVals + idx, mInfo + idx};
			}

			/*template <typename OtherKey>
			iterator find(const OtherKey& key, is_transparent_tag) {
				const size_t idx = findIdx(key);
				return iterator {mKeyVals + idx, mInfo + idx};
			}

			template <typename OtherKey, typename Self_ = Self>
			iterator find(const OtherKey& key) requires IsTransparent<Self_> {
				const size_t idx = findIdx(key);
				return iterator {mKeyVals + idx, mInfo + idx};
			}*/

			/// Get the beginning of internal data											
			///	@return the iterator															
			iterator begin() {
				if (IsEmpty())
					return end();
				return iterator(mKeyVals, mInfo, fast_forward_tag {});
			}

			/// Get the beginning of internal data (const)								
			///	@return the iterator															
			const_iterator begin() const {
				return cbegin();
			}

			const_iterator cbegin() const {
				if (IsEmpty())
					return cend();
				return const_iterator(mKeyVals, mInfo, fast_forward_tag {});
			}

			iterator end() {
				// no need to supply valid info pointer: end() must not be dereferenced, and only node
				// pointer is compared.
				return iterator {reinterpret_cast_no_cast_align_warning<Node*>(mInfo), nullptr};
			}

			const_iterator end() const {
				return cend();
			}

			const_iterator cend() const {
				return const_iterator {reinterpret_cast_no_cast_align_warning<Node*>(mInfo), nullptr};
			}

			iterator erase(const_iterator pos) {
				// its safe to perform const cast here
				return erase(iterator {const_cast<Node*>(pos.mKeyVals), const_cast<uint8_t*>(pos.mInfo)});
			}

			/// Erases element at pos, returns iterator to the next element		
			iterator erase(iterator pos) {
				// we assume that pos always points to a valid entry, and not end().
				auto const idx = static_cast<size_t>(pos.mKeyVals - mKeyVals);
				shiftDown(idx);
				--mNumElements;

				if (*pos.mInfo) {
					// we've backward shifted, return this again
					return pos;
				}

				// no backward shift, return next element
				return ++pos;
			}

			/// Erase a pair																		
			///	@param key - the key to search for										
			///	@return the number of removed pairs										
			size_t erase(const key_type& key) {
				static_assert(IsComparable<Key>, "Can't compare keys");
				size_t idx {};
				InfoType info {};
				keyToIdx(key, &idx, &info);

				// Check while info matches with the source idx					
				do {
					if (info == mInfo[idx] && key == mKeyVals[idx].getFirst()) {
						shiftDown(idx);
						--mNumElements;
						return 1;
					}
					next(&info, &idx);
				} while (info <= mInfo[idx]);

				// Nothing found to delete												
				return 0;
			}

			/// Reserves space for the specified number of elements. Makes sure	
			/// the old data fits. Exactly the same as reserve(c)						
			void rehash(size_t c) {
				// forces a reserve
				Allocate(c, true);
			}

			/// Reserves space for the specified number of elements. Makes sure	
			/// the old data fits. Exactly the same as rehash(c). Use rehash(0)	
			/// to shrink to fit																	
			void Allocate(size_t c) {
				// Reserve, but don't force rehash									
				Allocate(c, false);
			}

			/// If possible reallocates the map to a smaller one. This frees the	
			/// underlying table. Does not do anything if load_factor is too		
			/// large for decreasing the table's size.									
			void compact() {
				auto newSize = InitialNumElements;
				while (calcMaxNumElementsAllowed(newSize) < mNumElements && newSize != 0)
					newSize *= 2;

				if (LANGULUS_UNLIKELY(newSize == 0))
					throwOverflowError();

				// Only actually do anything when the new size is bigger		
				// than the old one. This prevents to continuously allocate	
				// for each reserve() call												
				if (newSize < mMask + 1)
					rehashPowerOfTwo(newSize, true);
			}

			NOD() constexpr Count GetCount() const noexcept {
				return mNumElements;
			}

			NOD() constexpr Count max_size() const noexcept {
				return static_cast<Count>(-1);
			}

			NOD() constexpr bool IsEmpty() const noexcept {
				return 0 == mNumElements;
			}

			NOD() constexpr float max_load_factor() const noexcept {
				return MaxLoadFactor100 / 100.0f;
			}

			/// Average number of elements per bucket. Since we allow only 1		
			/// per bucket																			
			NOD() constexpr float load_factor() const noexcept {
				return static_cast<float>(GetCount()) / static_cast<float>(mMask + 1);
			}

			NOD() size_t mask() const noexcept {
				return mMask;
			}

			NOD() size_t calcMaxNumElementsAllowed(size_t maxElements) const noexcept {
				if (LANGULUS_LIKELY(maxElements <= (std::numeric_limits<size_t>::max)() / 100))
					return maxElements * MaxLoadFactor100 / 100;

				// we might be a bit inprecise, but since maxElements is quite large that doesn't matter
				return (maxElements / 100) * MaxLoadFactor100;
			}

			NOD() size_t calcNumBytesInfo(size_t numElements) const noexcept {
				// we add a uint64_t, which houses the sentinel (first byte) and padding so we can load
				// 64bit types.
				return numElements + sizeof(uint64_t);
			}

			NOD() size_t calcNumElementsWithBuffer(size_t numElements) const noexcept {
				auto maxNumElementsAllowed = calcMaxNumElementsAllowed(numElements);
				return numElements + (std::min) (maxNumElementsAllowed, (static_cast<size_t>(0xFF)));
			}

			/// Calculation only allowed for 2^n values									
			NOD() size_t calcNumBytesTotal(size_t numElements) const {
				#if LANGULUS(BITNESS) == 64
					return numElements * sizeof(Node) + calcNumBytesInfo(numElements);
				#else
					// make sure we're doing 64bit operations, so we are at least safe against 32bit overflows.
					auto const ne = static_cast<uint64_t>(numElements);
					auto const s = static_cast<uint64_t>(sizeof(Node));
					auto const infos = static_cast<uint64_t>(calcNumBytesInfo(numElements));

					auto const total64 = ne * s + infos;
					auto const total = static_cast<size_t>(total64);

					if (LANGULUS_UNLIKELY(static_cast<uint64_t>(total) != total64))
						throwOverflowError();

					return total;
				#endif
			}

		private:
			template <ReflectedData Q = mapped_type>
			NOD() bool has(const value_type& e) const {
				auto it = find(e.first);
				return it != end() && it->second == e.second;
			}

			template <IsVoid Q = mapped_type>
			NOD() bool has(const value_type& e) const {
				return find(e) != end();
			}

			/// Allocate memory for the map													
			///	@param c - number of elements to allocate								
			///	@param forceRehash - force rehash										
			void Allocate(size_t c, bool forceRehash) {
				auto const minElementsAllowed = (std::max) (c, mNumElements);
				auto newSize = InitialNumElements;
				while (calcMaxNumElementsAllowed(newSize) < minElementsAllowed && newSize != 0)
					newSize *= 2;

				if (LANGULUS_UNLIKELY(newSize == 0))
					throwOverflowError();

				// Only actually do anything when the new size is bigger		
				// than the old one. This prevents to continuously allocate	
				// for each reserve() call.											
				if (forceRehash || newSize > mMask + 1)
					rehashPowerOfTwo(newSize, false);
			}

			/// Reserves space for at least the specified number of elements.		
			/// Only works if numBuckets if power of two. True on success			
			void rehashPowerOfTwo(size_t numBuckets, bool forceFree) {
				Node* const oldKeyVals = mKeyVals;
				uint8_t const* const oldInfo = mInfo;
				const size_t oldMaxElementsWithBuffer = calcNumElementsWithBuffer(mMask + 1);

				// resize operation: move stuff
				initData(numBuckets);
				if (oldMaxElementsWithBuffer > 1) {
					for (size_t i = 0; i < oldMaxElementsWithBuffer; ++i) {
						if (oldInfo[i] != 0) {
							// might throw an exception, which is really bad since we are in the middle of
							// moving stuff.
							insert_move(std::move(oldKeyVals[i]));
							// destroy the node but DON'T destroy the data.
							oldKeyVals[i].~Node();
						}
					}

					// this check is not necessary as it's guarded by the previous if, but it helps
					// silence g++'s overeager "attempt to free a non-heap object 'map'
					// [-Werror=free-nonheap-object]" warning.
					if (oldKeyVals != reinterpret_cast_no_cast_align_warning<Node*>(&mMask)) {
						// don't destroy old data: put it into the pool instead
						if (forceFree)
							std::free(oldKeyVals);
						else
							DataPool::addOrFree(oldKeyVals, calcNumBytesTotal(oldMaxElementsWithBuffer));
					}
				}
			}

			LANGULUS(NOINLINE) void throwOverflowError() const {
				throw std::overflow_error("robin_hood::map overflow");
			}

			template <typename OtherKey, typename... Args>
			std::pair<iterator, bool> try_emplace_impl(OtherKey&& key, Args&&... args) {
				auto idxAndState = insertKeyPrepareEmptySpot(key);
				switch (idxAndState.second) {
				case InsertionState::key_found:
					break;

				case InsertionState::new_node:
					::new (static_cast<void*>(&mKeyVals[idxAndState.first])) Node(
						*this, std::piecewise_construct, std::forward_as_tuple(std::forward<OtherKey>(key)),
						std::forward_as_tuple(std::forward<Args>(args)...));
					break;

				case InsertionState::overwrite_node:
					mKeyVals[idxAndState.first] = Node(*this, std::piecewise_construct,
						std::forward_as_tuple(std::forward<OtherKey>(key)),
						std::forward_as_tuple(std::forward<Args>(args)...));
					break;

				case InsertionState::overflow_error:
					throwOverflowError();
					break;
				}

				return std::make_pair(iterator(mKeyVals + idxAndState.first, mInfo + idxAndState.first),
					InsertionState::key_found != idxAndState.second);
			}

			template <typename OtherKey, typename Mapped>
			std::pair<iterator, bool> insertOrAssignImpl(OtherKey&& key, Mapped&& obj) {
				auto idxAndState = insertKeyPrepareEmptySpot(key);
				switch (idxAndState.second) {
				case InsertionState::key_found:
					mKeyVals[idxAndState.first].getSecond() = std::forward<Mapped>(obj);
					break;

				case InsertionState::new_node:
					::new (static_cast<void*>(&mKeyVals[idxAndState.first])) Node(
						*this, std::piecewise_construct, std::forward_as_tuple(std::forward<OtherKey>(key)),
						std::forward_as_tuple(std::forward<Mapped>(obj)));
					break;

				case InsertionState::overwrite_node:
					mKeyVals[idxAndState.first] = Node(*this, std::piecewise_construct,
						std::forward_as_tuple(std::forward<OtherKey>(key)),
						std::forward_as_tuple(std::forward<Mapped>(obj)));
					break;

				case InsertionState::overflow_error:
					throwOverflowError();
					break;
				}

				return std::make_pair(iterator(mKeyVals + idxAndState.first, mInfo + idxAndState.first),
					InsertionState::key_found != idxAndState.second);
			}

			void initData(size_t max_elements) {
				mNumElements = 0;
				mMask = max_elements - 1;
				mMaxNumElementsAllowed = calcMaxNumElementsAllowed(max_elements);

				auto const numElementsWithBuffer = calcNumElementsWithBuffer(max_elements);

				// malloc & zero mInfo. Faster than calloc everything.
				auto const numBytesTotal = calcNumBytesTotal(numElementsWithBuffer);
				mKeyVals = reinterpret_cast<Node*>(Inner::assertNotNull<std::bad_alloc>(std::malloc(numBytesTotal)));
				mInfo = reinterpret_cast<uint8_t*>(mKeyVals + numElementsWithBuffer);
				std::memset(mInfo, 0, numBytesTotal - numElementsWithBuffer * sizeof(Node));

				// set sentinel
				mInfo[numElementsWithBuffer] = 1;

				mInfoInc = InitialInfoInc;
				mInfoHashShift = InitialInfoHashShift;
			}

			enum class InsertionState {
				overflow_error,
				key_found,
				new_node,
				overwrite_node
			};

			/// Finds key, and if not already present prepares a spot where to	
			/// pot the key & value. This potentially shifts nodes out of the		
			/// way, updates mInfo and number of inserted elements, so the only	
			/// operation left to do is create/assign a new node at that spot		
			template <typename OtherKey>
			std::pair<size_t, InsertionState> insertKeyPrepareEmptySpot(OtherKey&& key) {
				for (int i = 0; i < 256; ++i) {
					size_t idx {};
					InfoType info {};
					keyToIdx(key, &idx, &info);
					nextWhileLess(&info, &idx);

					// While we potentially have a match							
					while (info == mInfo[idx]) {
						static_assert(IsComparable<Key, OtherKey>, "Can't compare keys");
						if (key == mKeyVals[idx].getFirst()) {
							// Key already exists, do NOT insert					
							return std::make_pair(idx, InsertionState::key_found);
						}

						next(&info, &idx);
					}

					// Unlikely that this evaluates to true						
					if (LANGULUS_UNLIKELY(mNumElements >= mMaxNumElementsAllowed)) {
						if (!increase_size())
							return std::make_pair(size_t(0), InsertionState::overflow_error);

						continue;
					}

					// Key not found, so we are now exactly where we want to	
					// insert it															
					auto const insertion_idx = idx;
					auto const insertion_info = info;
					if (LANGULUS_UNLIKELY(insertion_info + mInfoInc > 0xFF))
						mMaxNumElementsAllowed = 0;

					// find an empty spot
					while (0 != mInfo[idx])
						next(&info, &idx);

					if (idx != insertion_idx)
						shiftUp(idx, insertion_idx);

					// put at empty spot
					mInfo[insertion_idx] = static_cast<uint8_t>(insertion_info);
					++mNumElements;
					return std::make_pair(insertion_idx, idx == insertion_idx
						? InsertionState::new_node
						: InsertionState::overwrite_node);
				}

				// enough attempts failed, so finally give up.
				return std::make_pair(size_t(0), InsertionState::overflow_error);
			}

			bool try_increase_info() {
				if (mInfoInc <= 2) {
					// need to be > 2 so that shift works (otherwise undefined behavior!)
					return false;
				}

				// we got space left, try to make info smaller
				mInfoInc = static_cast<uint8_t>(mInfoInc >> 1U);

				// remove one bit of the hash, leaving more space for the distance info.
				// This is extremely fast because we can operate on 8 bytes at once.
				++mInfoHashShift;
				auto const numElementsWithBuffer = calcNumElementsWithBuffer(mMask + 1);

				for (size_t i = 0; i < numElementsWithBuffer; i += 8) {
					auto val = unaligned_load<uint64_t>(mInfo + i);
					val = (val >> 1U) & UINT64_C(0x7f7f7f7f7f7f7f7f);
					std::memcpy(mInfo + i, &val, sizeof(val));
				}

				// update sentinel, which might have been cleared out!
				mInfo[numElementsWithBuffer] = 1;

				mMaxNumElementsAllowed = calcMaxNumElementsAllowed(mMask + 1);
				return true;
			}

			/// True if resize was possible, false otherwise							
			bool increase_size() {
				// nothing allocated yet? just allocate InitialNumElements
				if (0 == mMask) {
					initData(InitialNumElements);
					return true;
				}

				auto const maxNumElementsAllowed = calcMaxNumElementsAllowed(mMask + 1);
				if (mNumElements < maxNumElementsAllowed && try_increase_info()) {
					return true;
				}

				if (mNumElements * 2 < calcMaxNumElementsAllowed(mMask + 1)) {
					// we have to resize, even though there would still be plenty of space left!
					// Try to rehash instead. Delete freed memory so we don't steadyily increase mem in case
					// we have to rehash a few times
					nextHashMultiplier();
					rehashPowerOfTwo(mMask + 1, true);
				}
				else {
					// we've reached the capacity of the map, so the hash seems to work nice. Keep using it.
					rehashPowerOfTwo((mMask + 1) * 2, false);
				}

				return true;
			}

			void nextHashMultiplier() {
				// adding an *even* number, so that the multiplier will always stay odd. This is necessary
				// so that the hash stays a mixing function (and thus doesn't have any information loss).
				mHashMultiplier += UINT64_C(0xc4ceb9fe1a85ec54);
			}

			void destroy() {
				if (0 == mMask) {
					// don't deallocate!
					return;
				}

				DestroyNodes<false>();

				// This protection against not deleting mMask shouldn't be needed as it's sufficiently
				// protected with the 0==mMask check, but I have this anyways because g++ 7 otherwise
				// reports a compile error: attempt to free a non-heap object 'fm'
				// [-Werror=free-nonheap-object]
				if (mKeyVals != reinterpret_cast_no_cast_align_warning<Node*>(&mMask))
					std::free(mKeyVals);
			}

			void init() noexcept {
				mKeyVals = reinterpret_cast_no_cast_align_warning<Node*>(&mMask);
				mInfo = reinterpret_cast<uint8_t*>(&mMask);
				mNumElements = 0;
				mMask = 0;
				mMaxNumElementsAllowed = 0;
				mInfoInc = InitialInfoInc;
				mInfoHashShift = InitialInfoHashShift;
			}

		};

	} // namespace detail

	/// Map																							
	template <class K, class V, Count MaxLoadFactor100 = 80>
	using unordered_flat_map = Inner::Table<true, MaxLoadFactor100, K, V>;

	template <class K, class V, Count MaxLoadFactor100 = 80>
	using unordered_node_map = Inner::Table<false, MaxLoadFactor100, K, V>;

	template <class K, class V, Count MaxLoadFactor100 = 80>
	using unordered_map = Inner::Table<sizeof(TPair<K, V>) <= sizeof(Count) * 6 && std::is_nothrow_move_constructible_v<TPair<K, V>> && std::is_nothrow_move_assignable_v<TPair<K, V>>, MaxLoadFactor100, K, V>;

	/// Set																							
	template <class K, Count MaxLoadFactor100 = 80>
	using unordered_flat_set = Inner::Table<true, MaxLoadFactor100, K, void>;

	template <class K, Count MaxLoadFactor100 = 80>
	using unordered_node_set = Inner::Table<false, MaxLoadFactor100, K, void>;

	template <class K, Count MaxLoadFactor100 = 80>
	using unordered_set = Inner::Table<sizeof(K) <= sizeof(Count) * 6 && std::is_nothrow_move_constructible_v<K> && std::is_nothrow_move_assignable_v<K>, MaxLoadFactor100, K, void>;

} // namespace Langulus::Anyness

#include "THashMap.inl"

#undef NODE_TEMPLATE
#undef NODE

#undef TABLE_TEMPLATE
#undef TABLE
