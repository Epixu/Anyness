#include "Allocator.hpp"
#include "Exceptions.hpp"
#include "Reflection.hpp"

namespace Langulus::Anyness
{

	constexpr auto Padding = LANGULUS_ALIGN() + sizeof(void*);
	
	/// MSVC will likely never support std::aligned_alloc, so we use				
	/// a custom portable routine that's almost the same								
	/// https://stackoverflow.com/questions/62962839									
	///																								
	/// Each allocation has the following prefixed bytes:								
	///	[padding for alignment][void*][Entry][allocated bytes...]				
	///																								
	///	@param size - the number of bytes to allocate								
	///	@return a newly allocated memory that is correctly aligned				
	///	@attention you are responsible for deallocating via AlignedFree 		
	inline Entry* AlignedAllocate(const Size& size) {
		auto originalAddress = malloc(Padding + Entry::GetSize() + size);
		if (!originalAddress)
			throw Except::Allocate(Logger::Error() << "Out of memory");

		void** ptr = reinterpret_cast<void**>(
			(reinterpret_cast<Size>(originalAddress) + Padding) & ~(LANGULUS_ALIGN() - 1)
		);
		
		// Save the original address relative to the entry, so that we		
		// can easily get to it when freeing the memory							
		ptr[-1] = originalAddress;
		new (ptr) Entry {size, nullptr};
		return reinterpret_cast<Entry*>(ptr);
	}

	/// Free aligned memory that has been allocated via AlignedMalloc				
	///	@param ptr - the aligned pointer to free										
	///					 must've been prior allocated via AlignedMalloc				
	inline void AlignedFree(Entry* ptr) noexcept {
		free(reinterpret_cast<void**>(ptr)[-1]);
	}
	
	/// Allocate a memory entry																
	///	@attention doesn't call any constructors										
	///	@param size - the number of bytes to allocate								
	///	@return the allocated memory entry												
	Entry* Allocator::Allocate(Size size) {
		return AlignedAllocate(size);
	}

	/// Reallocate a memory entry																
	/// This actually works only when MANAGED_MEMORY feature is enabled			
	///	@attention never calls any constructors										
	///	@attention never copies any data													
	///	@attention never deallocates previous entry									
	///	@param size - the number of bytes to allocate								
	///	@param previous - the previous memory entry									
	///	@return the reallocated memory entry											
	Entry* Allocator::Reallocate(Size size, Entry* previous) {
		#if LANGULUS_FEATURE(MANAGED_MEMORY)
			TODO(); // attempt to reallocate inside the same place in pool
		#else
			// Forget about anything else, realloc is bad design				
			return AlignedAllocate(size);
		#endif
	}
	
	/// Deallocate a memory allocation														
	///	@attention doesn't call any destructors										
	///	@param meta - the type of data to deallocate (optional)					
	///	@param entry - the memory entry to deallocate								
	void Allocator::Deallocate(DMeta meta, Entry* entry) {
		AlignedFree(entry);
	}

	/// Find a memory entry from pointer													
	/// If LANGULUS_FEATURE(MANAGED_MEMORY) is enabled, this function will		
	/// attempt to find memory entry from the memory manager							
	/// Allows us to safely interface unknown memory, possibly reusing it		
	///	@param meta - the type of data to search for (optional)					
	///	@param memory - memory pointer													
	///	@return the reallocated memory entry											
	Entry* Allocator::Find(DMeta meta, const void* memory) {
		#if LANGULUS_FEATURE(MANAGED_MEMORY)
			TODO();
		#else
			(meta); (memory);
			return nullptr;
		#endif
	}

	/// Check if memory is owned by the memory manager									
	/// Unlike Allocator::Find, this doesn't check if memory is currently used	
	///	@attention this function does nothing if										
	///              LANGULUS_FEATURE(MANAGED_MEMORY) is disabled. This has		
	///				  dire consequences on sparse containers, since one can not	
	///				  determine if a pointer is owned or not without it!			
	///	@param meta - the type of data to search for (optional)					
	///	@param memory - memory pointer													
	///	@return true if we own the memory												
	bool Allocator::CheckAuthority(DMeta meta, const void* memory) {
		#if LANGULUS_FEATURE(MANAGED_MEMORY)
			TODO();
		#else
			(meta); (memory);
			return false;
		#endif
	}
	
	/// Get the number of uses a memory entry has										
	///	@attention this function does nothing if										
	///              LANGULUS_FEATURE(MANAGED_MEMORY) is disabled. This has		
	///				  dire consequences on sparse containers, since one can not	
	///				  determine if a pointer is owned or not without it!			
	///	@param meta - the type of data to search for (optional)					
	///	@param memory - memory pointer													
	///	@return the number of references, or 1 if memory is not ours			
	Count Allocator::GetReferences(DMeta meta, const void* memory) {
		#if LANGULUS_FEATURE(MANAGED_MEMORY)
			TODO();
		#else
			(meta); (memory);
			return 1;
		#endif
	}
	
	/// Reference some memory, which we do not know if owned or not				
	/// If LANGULUS_FEATURE(MANAGED_MEMORY) is enabled, this function will		
	/// attempt to find memory entry from the memory manager and reference it	
	///	@attention this function does nothing if										
	///              LANGULUS_FEATURE(MANAGED_MEMORY) is disabled. This has		
	///				  dire consequences on sparse containers, since one can not	
	///				  determine if a pointer is owned or not without it!			
	///	@param meta - the type of data to search for (optional)					
	///	@param memory - memory pointer													
	///	@param count - the number of references to add								
	void Allocator::Keep(DMeta meta, const void* memory, Count count) {
		#if LANGULUS_FEATURE(MANAGED_MEMORY)
			auto found = Find(meta, memory);
			if (found)
				found->mReferences += count;
		#else
			(meta); (memory); (count);
		#endif
	}

	/// Dereference some memory, which we do not know if owned or not				
	/// If LANGULUS_FEATURE(MANAGED_MEMORY) is enabled, this function will		
	/// attempt to find memory entry from the memory manager and dereference	
	///	@attention this function does nothing if										
	///              LANGULUS_FEATURE(MANAGED_MEMORY) is disabled. This has		
	///				  dire consequences on sparse containers, since one can not	
	///				  determine if a pointer is owned or not without it!			
	///	@attention this will deallocate memory if fully dereferenced			
	///				  which is troublesome if you need to call destructors		
	///				  Won't deallocate if LANGULUS_FEATURE(MANAGED_MEMORY) is	
	///				  disabled																	
	///	@param meta - the type of data to search for (optional)					
	///	@param memory - memory pointer													
	///	@param count - the number of references to add								
	///	@return true if the memory has been fully dereferenced					
	bool Allocator::Free(DMeta meta, const void* memory, Count count) {
		#if LANGULUS_FEATURE(MANAGED_MEMORY)
			auto found = Find(meta, memory);
			if (!found)
				// Data is either static or unallocated - don't touch it		
				return false;

			if (found->mReferences <= times) {
				// Deallocate the entry													
				Deallocate(meta, found);
				return true;
			}

			found->mReferences -= count;
			return false;
		#else
			(meta); (memory); (count);
			return false;
		#endif
	}
	
	Allocator::Statistics Allocator::mStatistics {};
	
	/// Get allocator statistics																
	///	@return a reference to the statistics structure								
	const Allocator::Statistics& Allocator::GetStatistics() noexcept {
		return mStatistics;
	}

} // namespace Langulus::Anyness
