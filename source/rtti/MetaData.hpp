#pragma once
#include "Meta.hpp"
#include <Langulus/HashOf.hpp>
#include <Langulus/IntentOf.hpp>


namespace Langulus::RTTI
{
   class DefinitionData;

   namespace Inner
   {
   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      /// Relies on the definition limits to pack an ID into the smallest     
      /// possible space, but also uses some additional bits to encode some   
      /// often used information about the definition. The handle still has   
      /// to be transformed into a pointer for more advanced uses, but in     
      /// general it is likely to avoid an indirection altogether at the      
      /// cost of a bitwise operation, making it a bit more cache-friendly,   
      /// and worth experimenting with                                        
      struct MetaDataStructured_8_8 : MetaPacked<1> {

      };

      struct MetaDataStructured_16_16 : MetaPacked<2> {

      };

      struct MetaDataStructured_24_8 : MetaPacked<3> {

      };

      struct MetaDataStructured_32_8 : MetaPacked<4> {

      };

      struct MetaDataStructured_32_16 : MetaPacked<4> {

      };
   #endif

      /// A naked pointer to a definition. Probably the fastest, but most     
      /// memory-inefficient on 64bit systems                                 
      struct MetaDataNaked : MetaNaked<DefinitionData> {
         using MetaNaked<DefinitionData>::MetaNaked;
         using MetaNaked<DefinitionData>::operator =;
         using MetaNaked<DefinitionData>::operator bool;

         template<class, class...>
         bool Is() const noexcept;
         bool Is(const MetaDataNaked&) const noexcept;

         template<class, class...>
         bool IsSimilar() const noexcept;
         bool IsSimilar(const MetaDataNaked&) const noexcept;

         template<class, class...>
         bool IsExact() const noexcept;

         ::std::size_t GetMinAllocation() const noexcept;
         ::std::size_t GetSize() const noexcept;
         ::std::size_t GetAlignment() const noexcept;
         Token GetName() const noexcept;

         bool IsDense() const noexcept;
         bool IsSparse() const noexcept;
         bool IsConstant() const noexcept;
         bool IsMutable() const noexcept;
         bool IsDeep() const noexcept;
         bool IsPOD() const noexcept;

         bool HasReferConstructor() const noexcept;
         void RunReferConstructor(const void*, void*) const;
         bool HasReferAssigner() const noexcept;
         void RunReferAssigner(const void*, void*) const;

         bool HasMoveConstructor() const noexcept;
         void RunMoveConstructor(void*, void*) const;
         bool HasMoveAssigner() const noexcept;
         void RunMoveAssigner(void*, void*) const;

         bool HasAbandonConstructor() const noexcept;
         void RunAbandonConstructor(void*, void*) const;
         bool HasAbandonAssigner() const noexcept;
         void RunAbandonAssigner(void*, void*) const;

         bool HasDisownConstructor() const noexcept;
         void RunDisownConstructor(const void*, void*) const;
         bool HasDisownAssigner() const noexcept;
         void RunDisownAssigner(const void*, void*) const;

         bool HasCloneConstructor() const noexcept;
         void RunCloneConstructor(const void*, void*) const;
         bool HasCloneAssigner() const noexcept;
         void RunCloneAssigner(const void*, void*) const;

         bool HasCopyConstructor() const noexcept;
         void RunCopyConstructor(const void*, void*) const;
         bool HasCopyAssigner() const noexcept;
         void RunCopyAssigner(const void*, void*) const;

         bool HasComparer() const noexcept;
         int  RunComparer(const void*, const void*) const;

         bool HasHasher() const noexcept;
         bool HasGetHashMethod() const noexcept;
         Hash RunHasher(const void*) const;
      };

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      using MetaDataBase = MetaDataStructured_16_16;
   #else
      using MetaDataBase = MetaDataNaked;
   #endif

   } // namespace Langulus::RTTI::Inner
   

   ///                                                                        
   ///   Data type ID                                                         
   ///                                                                        
   /// Can be a naked pointer to a definition, or a structured ID that is     
   /// either packed to a smaller size, or carry a lot of meta information    
   /// in the ID itself to avoid indirection                                  
   ///                                                                        
   struct MetaData : Inner::MetaDataBase {
      using CTTI_POD      = Yes;
      using CTTI_Nullable = Yes;

      ignore_all_intents(MetaData);

      using Inner::MetaDataBase::MetaDataBase;
      using Inner::MetaDataBase::operator =;
      using Inner::MetaDataBase::operator bool;
   };

   using DMeta = MetaData;

} // namespace Langulus::RTTI