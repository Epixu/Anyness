#pragma once
#include "Definition.hpp"
#include "MetaData.hpp"


namespace Langulus::RTTI
{

   ///                                                                        
   /// A data definition                                                      
   ///                                                                        
   class DefinitionData : public Inner::Definition {
   protected:
      // The origin type, with all qualifiers and sparseness removed    
      // Will be nullptr for incomplete types                           
      MetaData mOrigin;
      // The type, when a single pointer is removed                     
      // Will be null if data is dense                                  
      MetaData mDeptr;
      // A unique handle for this definition                            
      MetaData mThis;

      // Data instance size in bytes, set by sizeof()                   
      size_t mSize;
      // Data instance alignment in bytes, set by alignof()             
      size_t mAlign;
      // True if data is constant, set by CT::Constant                  
      bool mConst;
      // Minimal pool allocation, in bytes                              
      size_t mAllocationPage;
      // Precomputed counts indexed by MSB (avoids division by stride)  
      size_t mAllocationTable[sizeof(size_t) * 8 + 1];

   public:
      friend struct MetaData;
      DefinitionData(const Token& cppname) : Definition {cppname} {}

      template<class>
      static DMeta Reflect();
   };

} // namespace Langulus::RTTI

#include "DefinitionData.inl"